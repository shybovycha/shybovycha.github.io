import java.time.Duration
import cats.effect.IO
import cats.data.Either

case class Currency(code: String)

trait HttpClient {
  def get(url: String): IO[Either[HttpError, HttpResponse]]
}

trait JsonParser {
  def parseJson(response: HttpResponse): Either[HttpError, Json]
}

trait Cache[F[_], K, V] {
  def get(key: K): F[Option[V]]
  def put(key: K, value: V): F[Unit]
  def invalidateAll(): F[Unit]
}

trait Scheduler[F[_]] {
  def schedule(delay: Duration, task: F[Unit]): F[Unit]
}

def forexExchange(from: Currency, to: Currency)(implicit C: Cache[IO, (Currency, Currency), BigDecimal], S: Scheduler[IO]) = {
  C.get((from, to)) match {
    case Some(rate) => IO.pure(makeHtml(rate, from, to))
    case None =>
      makeHttpCall("http://forex.com/convert/${from}/${to}")
        .andThen(response => parseForexJson(response).andThen(json => getExchangeRate(json).andThenThru(rate => C.put((from, to), rate)).andThenThru(rate => S.schedule(1.hour, C.invalidateAll())).andThen(rate => makeHtml(rate, from, to))))
  }
}

def makeHttpCall(url: String)(implicit H: HttpClient): IO[Either[HttpError, HttpResponse]] = H.get(url)

def parseForexJson(response: HttpResponse)(implicit P: JsonParser): Either[HttpError, Json] = P.parseJson(response)

def getExchangeRate(json: Json): Option[BigDecimal] = json.get("rate")

def makeHtml(rate: Double, from: Currency, to: Currency) = s"<h1>1 $from = $rate $to</h1>"
