/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.smartstub

import scala.collection.concurrent.TrieMap
import java.time.LocalDate
import org.scalacheck._

trait Generator[IN,OUT] {
  val Gen = org.scalacheck.Gen
  val state = TrieMap.empty[IN,OUT]
  def apply(in: IN): Option[OUT] = {state get in} orElse generate(in)
  def generate(in: IN): Option[OUT] =
    from(in).flatMap{x => generator(in)(Gen.Parameters.default, rng.Seed(x))}

  def update(key: IN, value: OUT): Unit = state(key) = value 

  def generator(in: IN): Gen[OUT]

  def dateGen(
    start: Int = 1970,
    end: Int = 2000
  ): Gen[LocalDate] = dateGen(
    LocalDate.of(start, 1, 1),
    LocalDate.of(end, 12, 31)
  )

  def dateGen(start: LocalDate, end: LocalDate): Gen[LocalDate] = 
    Gen.choose(start.toEpochDay, end.toEpochDay).map(LocalDate.ofEpochDay)

  def from(in: IN): Option[Long]

}

object Generator {
  def auto[A,B](implicit toView: ToLong[A], fromView: FromLong[B]): Generator[A,B] = new Generator[A,B] {
    def from(in: A): Option[Long] = Some(toView.asLong(in))
    def generator(in: A): org.scalacheck.Gen[B] = fromView.gen
  }

  def auto[A,B](f: A => Gen[B])(implicit toView: ToLong[A]): Generator[A,B] = new Generator[A,B] {
    def from(in: A): Option[Long] = Some(toView.asLong(in))
    def generator(in: A): Gen[B] = f(in)
  }

}
