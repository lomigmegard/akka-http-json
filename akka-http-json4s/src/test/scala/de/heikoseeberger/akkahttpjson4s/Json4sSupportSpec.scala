/*
 * Copyright 2015 Heiko Seeberger
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

package de.heikoseeberger.akkahttpjson4s

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.{HttpEntity, RequestEntity}
import akka.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException
import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import akka.stream.ActorMaterializer
import org.json4s.{DefaultFormats, jackson, native}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

object Json4sSupportSpec {
  case class Foo(bar: String) { require(bar == "bar", "bar must be 'bar'!") }
}

class Json4sSupportSpec extends WordSpec with Matchers with ScalaFutures with BeforeAndAfterAll {
  import Json4sSupport._
  import Json4sSupportSpec._

  implicit val system = ActorSystem()
  implicit val mat = ActorMaterializer()
  implicit val formats = DefaultFormats

  val foo = Foo("bar")

  "Json4sSupport" should {
    import system.dispatcher

    "enable marshalling and unmarshalling objects for `DefaultFormats` and `jackson.Serialization`" in {
      implicit val serialization = jackson.Serialization
      val entity = Marshal(foo).to[RequestEntity].futureValue
      entity.contentType shouldBe `application/json`
      Unmarshal(entity).to[Foo].futureValue shouldBe foo
    }

    "enable marshalling and unmarshalling objects for `DefaultFormats` and `native.Serialization`" in {
      implicit val serialization = native.Serialization
      val entity = Marshal(foo).to[RequestEntity].futureValue
      entity.contentType shouldBe `application/json`
      Unmarshal(entity).to[Foo].futureValue shouldBe foo
    }

    "provide proper error messages for requirement errors" in {
      implicit val serialization = native.Serialization
      val entity = HttpEntity(`application/json`, """{ "bar": "baz" }""")
      val ex = Unmarshal(entity).to[Foo].failed.futureValue
      ex shouldBe an [IllegalArgumentException]
      ex should have message "requirement failed: bar must be 'bar'!"
    }

    "throw NoContentException for empty entities" in {
      implicit val serialization = native.Serialization
      val entity = HttpEntity.empty(`application/json`)
      val ex = Unmarshal(entity).to[Foo].failed.futureValue
      ex shouldBe Unmarshaller.NoContentException
      ex should have message "Message entity must not be empty"
    }

    "throw UnsupportedContentTypeException when Content-Type is not `application/json`" in {
      implicit val serialization = native.Serialization
      val entity = HttpEntity("""{ "bar": "bar" }""")
      val ex = Unmarshal(entity).to[Foo].failed.futureValue
      ex shouldBe an [UnsupportedContentTypeException]
      ex should have message "Unsupported Content-Type, supported: application/json"
    }
  }

  override protected def afterAll() = {
    system.terminate().futureValue
    super.afterAll()
  }
}
