import java.util.*

public sealed class Maybe<out T> {
  public abstract fun get(): T

  public fun<R> map(f: (T) -> R): Maybe<R> {
    return bind { Just(f(it)) }
  }

  // (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
  public fun<R> bind(f: (T) -> Maybe<R>): Maybe<R> {
    return if (this is Nothing) {
      Nothing
    } else {
      f(get())
    }
  }

  // (<*>)   :: (Applicative f) => f (a -> b) -> f a -> f b
  public fun<R> apply(f: Maybe<(T) -> R>): Maybe<R> {
    return if (f is Nothing) {
      Nothing
    } else {
      Just(f.get().invoke(this.get()))
    }
  }

  public object Nothing : Maybe<kotlin.Nothing>() {
    public override fun get() = throw NoSuchElementException("Nothing.get")

    override fun toString(): String {
      return "Nothing"
    }

    override fun equals(other: Any?): Boolean {
      return other is Nothing
    }

    override fun hashCode(): Int {
      return Integer.MAX_VALUE
    }
  }

  public class Just<out T>(val t: T) : Maybe<T>() {
    override fun get(): T {
      return t
    }

    override fun toString(): String {
      return "Just($t)"
    }

    override fun equals(other: Any?): Boolean {
      if (this === other) return true
      if (other?.javaClass != javaClass) return false

      other as Just<*>

      if (t != other.t) return false

      return true
    }

    override fun hashCode(): Int {
      return t?.hashCode() ?: 0
    }

  }
}