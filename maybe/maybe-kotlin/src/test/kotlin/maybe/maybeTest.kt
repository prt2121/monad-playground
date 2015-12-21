import Maybe.Just
import Maybe.Nothing
import org.junit.Test as test
import com.google.common.truth.Truth.assertThat

class maybeTest {

  @test fun testJust() {
    println(Just(5))
    assertThat(5).isEqualTo(Just(5).get())
  }

  @test fun testMap() {
    assertThat(Just(10)).isEqualTo(Just(5).map { it + 5 })
    assertThat(Nothing).isEqualTo(Nothing.map { it })
  }

  @test fun testBind() {
    fun tail(s: String): Maybe<String> {
      if (s.length < 2 )
        return Nothing
      else
        return Just(s.drop(1))
    }

    assertThat(Just("L")).isEqualTo(
        Just("LOL")
            .bind { tail(it) }
            .bind { tail(it) })

    assertThat(Nothing).isEqualTo(
        Just("LOL")
            .bind { tail(it) }
            .bind { tail(it) }
            .bind { tail(it) })

    assertThat(Just(1)).isEqualTo(
        Just("LOL")
            .bind { tail(it) }
            .bind { tail(it) }
            .map { it.length })

    assertThat(Nothing).isEqualTo(
        Just("LOL")
            .bind { tail(it) }
            .bind { tail(it) }
            .bind { tail(it) }
            .map { it.length })
  }

}