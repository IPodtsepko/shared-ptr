#pragma once

#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>

struct control_block_t
{
    control_block_t() = default;

    virtual ~control_block_t() = default;

    void add_strong_reference()
    {
        strong_reference_counter++;
        add_weak_reference();
    }

    void remove_strong_reference()
    {
        if (--strong_reference_counter == 0) {
            delete_object();
        }
        remove_weak_reference();
    }

    void add_weak_reference()
    {
        weak_reference_counter++;
    }

    void remove_weak_reference()
    {
        if (--weak_reference_counter == 0) {
            delete this;
        }
    }

    std::size_t use_count() const
    {
        return strong_reference_counter;
    }

    virtual void delete_object() = 0;

private:
    std::size_t strong_reference_counter = 1;
    std::size_t weak_reference_counter = 1;
};

template <typename T, typename Deleter>
struct pointer_control_block : control_block_t
{
    explicit pointer_control_block(T * object, Deleter && deleter)
        : object(object)
        , deleter(std::forward<Deleter>(deleter))
    {
    }

    void delete_object() override
    {
        deleter(object);
        object = nullptr;
    }

    ~pointer_control_block() override = default;

    T * object;
    [[no_unique_address]] Deleter deleter;
};

template <typename T>
struct inplace_control_block : control_block_t
{

    template <typename... Args>
    explicit inplace_control_block(Args &&... args)
    {
        new (&object) T(std::forward<Args>(args)...);
    }

    void delete_object() override
    {
        reinterpret_cast<T &>(object).~T();
    }

    ~inplace_control_block() override = default;

    typename std::aligned_storage<sizeof(T), alignof(T)>::type object;
};

template <typename>
class weak_ptr;

template <typename T>
class shared_ptr
{
public:
    template <typename>
    friend class shared_ptr;

    friend class weak_ptr<T>;

    template <typename U, typename... Args>
    friend shared_ptr<U> make_shared(Args &&...);

    shared_ptr() noexcept = default;

    explicit shared_ptr(std::nullptr_t) {}

    template <typename V,
              typename = std::enable_if_t<std::is_convertible_v<V, T>>,
              typename Deleter = std::default_delete<V>>
    explicit shared_ptr(V * object, Deleter && deleter = Deleter())
    try : control_block(new pointer_control_block(object, std::forward<Deleter>(deleter))), object(object) {
    }
    catch (...) {
        deleter(object);
        throw;
    }

    template <typename V,
              typename = std::enable_if_t<std::is_convertible_v<V, T>>>
    shared_ptr(const shared_ptr<V> & other) noexcept
        : control_block(other.control_block)
        , object(other.object)
    {
        add_strong_reference();
    }

    shared_ptr(const shared_ptr & other) noexcept
        : control_block(other.control_block)
        , object(other.object)
    {
        add_strong_reference();
    }

    shared_ptr(shared_ptr && other) noexcept
    {
        this->swap(other);
    }

    template <typename U>
    shared_ptr(shared_ptr<U> & other, T * object) noexcept
        : control_block(other.control_block)
        , object(object)
    {
        add_strong_reference();
    }

    shared_ptr & operator=(const shared_ptr & other) noexcept
    {
        if (this != &other) {
            link(other);
            add_strong_reference();
        }
        return *this;
    }

    shared_ptr & operator=(shared_ptr && other) noexcept
    {
        if (this != &other) {
            link(other);
            other.control_block = nullptr;
            other.object = nullptr;
        }
        return *this;
    }

    bool operator==(const std::nullptr_t &) const
    {
        return object == nullptr;
    }

    friend bool operator==(const std::nullptr_t &, const shared_ptr & rhs)
    {
        return rhs == nullptr;
    }

    bool operator!=(const std::nullptr_t &) const
    {
        return object != nullptr;
    }

    friend bool operator!=(const std::nullptr_t &, const shared_ptr & rhs)
    {
        return rhs != nullptr;
    }

    T * get() const noexcept
    {
        return object;
    }

    operator bool() const noexcept
    {
        return object != nullptr;
    }

    T & operator*() const noexcept
    {
        return *object;
    }

    T * operator->() const noexcept
    {
        return object;
    }

    std::size_t use_count() const noexcept
    {
        return control_block ? control_block->use_count() : 0;
    }

    void reset()
    {
        shared_ptr().swap(*this);
    }

    template <typename V,
              typename = std::enable_if_t<std::is_convertible_v<V, T>>,
              typename Deleter = std::default_delete<V>>
    void reset(V * ptr, Deleter && deleter = Deleter())
    {
        shared_ptr(ptr, std::forward<Deleter>(deleter)).swap(*this);
    }

    ~shared_ptr()
    {
        remove_strong_reference();
    }

private:
    shared_ptr(control_block_t * control_block, T * object) noexcept
        : control_block(control_block)
        , object(object)
    {
    }

    void remove_strong_reference()
    {
        if (control_block) {
            control_block->remove_strong_reference();
        }
    }

    void add_strong_reference()
    {
        if (control_block) {
            control_block->add_strong_reference();
        }
    }

    void link(const shared_ptr & other)
    {
        remove_strong_reference();

        control_block = other.control_block;
        object = other.object;
    }

    void swap(shared_ptr & other)
    {
        std::swap(control_block, other.control_block);
        std::swap(object, other.object);
    }

private:
    control_block_t * control_block = nullptr;
    T * object = nullptr;
};

template <typename T>
class weak_ptr
{
public:
    weak_ptr() noexcept = default;

    weak_ptr(const shared_ptr<T> & other) noexcept
    {
        *this = other;
    }

    weak_ptr(const weak_ptr<T> & other) noexcept
        : control_block(other.control_block)
        , object(other.object)
    {
        add_weak_reference();
    }

    ~weak_ptr()
    {
        remove_weak_reference();
    }

    weak_ptr & operator=(const shared_ptr<T> & other) noexcept
    {
        link(other);
        add_weak_reference();
        return *this;
    };

    weak_ptr & operator=(const weak_ptr & other) noexcept
    {
        if (this != &other) {
            link(other);
            add_weak_reference();
        }
        return *this;
    }

    weak_ptr & operator=(weak_ptr && other) noexcept
    {
        if (this != &other) {
            link(other);
            other.control_block = nullptr;
            other.object = nullptr;
        }
        return *this;
    }

    shared_ptr<T> lock() const noexcept
    {
        if (control_block && control_block->use_count() > 0) {
            control_block->add_strong_reference();
            return shared_ptr<T>(control_block, object);
        }
        return shared_ptr<T>();
    }

private:
    control_block_t * control_block = nullptr;
    T * object = nullptr;

    template <class SmartPtr>
    void link(const SmartPtr & other)
    {
        remove_weak_reference();
        control_block = other.control_block;
        object = other.object;
    }

    void remove_weak_reference()
    {
        if (control_block) {
            control_block->remove_weak_reference();
        }
    }

    void add_weak_reference()
    {
        if (control_block) {
            control_block->add_weak_reference();
        }
    }
};

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args &&... args)
{
    auto control_block = new inplace_control_block<T>(std::forward<Args>(args)...);
    return shared_ptr<T>(control_block, reinterpret_cast<T *>(&control_block->object));
}
