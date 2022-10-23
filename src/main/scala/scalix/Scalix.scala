package scalix

import org.json4s.*
import org.json4s.native.JsonMethods.*
import scalix.ConfigParam.{apiKey, url}
import scalix.MovieObjects.{FullName, Movies, Movie, People, Workers, Worker}

import scala.io.Source
import java.io.PrintWriter

object Scalix extends App {
  implicit val formats: Formats = DefaultFormats

  def findActorId(name: String, surname: String): Option[Int] = {
    val urlActor = url + s"search/person?query=${name}%20${surname}&api_key=${apiKey}"
    val person = getJsonData(urlActor).extract[People]
    Option.apply(person.results(0).id)
  }

  def findActorMovies(actorId: Int): Set[(Int, String)] = {
    val urlMovie = url + s"person/${actorId}/movie_credits?api_key=${apiKey}"
    val movies = getJsonData(urlMovie).extract[Movies]

//    val out = new PrintWriter(s"src/main/scala/data/actor$actorId.json")
//    out.print(movies)
//    out.flush()
////    val contents2 = Source.fromFile(s"src/main/scala/data/actor$actorId.json").mkString
    
    val response: Set[(Int, String)] = movies.cast.map {
      case m: Movie => (m.id, m.title)
    }
    response
  }

  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    val urlCredits = url + s"movie/${movieId}/credits?api_key=${apiKey}"
    val workers = getJsonData(urlCredits).extract[Workers]
    val director: List[Worker] = workers.crew.filter(_.job == "Director")
    Option.apply(director(0).id, director(0).name)
  }

  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
    val movie1 = findActorMovies(findActorId(actor1.name, actor1.surname).get)
    val movie2 = findActorMovies(findActorId(actor2.name, actor2.surname).get)
    val commonMovies: Set[(Int, String)] = movie1.filter((id, name) => movie2.contains(id, name))
    val response: Set[(String, String)] = commonMovies.map {
      (id, name) => (findMovieDirector(id).get._2, name)
    }
    response
  }

  private def getJsonData(url: String) = {
    val sourceName = Source.fromURL(url)
    val content = sourceName.mkString
    val json = parse(content)
    json
  }

}
