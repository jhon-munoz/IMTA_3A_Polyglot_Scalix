package scalix

import org.json4s.*
import org.json4s.native.JsonMethods.*
import scalix.ConfigParam.{actorPath, apiKey, extension, moviePath, url}
import scalix.MovieObjects.{FullName, Movie, Movies, People, Worker, Workers}

import scala.io.Source
import java.io.{File, FileReader, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable

object Scalix extends App {
  implicit val formats: Formats = DefaultFormats

  var cacheActorId: mutable.Map[(String, String), Int] = mutable.Map.empty
  var cacheActorMovies: mutable.Map[Int, Set[(Int, String)]] = mutable.Map.empty
  var cacheMovieDirector: mutable.Map[Int, Option[(Int, String)]] = mutable.Map.empty

  def findActorId(name: String, surname: String): Option[Int] = {
    val urlActor = url + s"search/person?query=${name}%20${surname}&api_key=${apiKey}"
    //Check primary cache
    var id = cacheActorId.get(name, surname)

    if (id.isEmpty) {
      val json = parse(Source.fromURL(urlActor).mkString)
      val people = json.extract[People]
      id = Option.apply(people.results(0).id)
      //save in primary cache
      cacheActorId += ((name, surname) -> id.get)
    }
    id
  }

  def findActorMovies(actorId: Int): Set[(Int, String)] = {
    val urlMovie = url + s"person/${actorId}/movie_credits?api_key=${apiKey}"
    var response = cacheActorMovies.get(actorId)

    if(response.isEmpty){
      val content = validateSecondaryCache(actorPath, actorId, urlMovie)
      val movies = parse(content).extract[Movies]

      response = Option.apply(movies.cast.map {
        case m: Movie => (m.id, m.title)
      })
      cacheActorMovies += (actorId -> response.get)
    }
    response.get
  }

  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    val urlCredits = url + s"movie/${movieId}/credits?api_key=${apiKey}"
    var respose = cacheMovieDirector.get(movieId)
    
    if(respose.isEmpty){
      val content = validateSecondaryCache(moviePath, movieId, urlCredits)
      val workers = parse(content).extract[Workers]

      val director: List[Worker] = workers.crew.filter(_.job == "Director")
      respose = Option.apply(Option.apply(director(0).id, director(0).name))
      cacheMovieDirector += (movieId -> respose.get)
    }
    respose.get
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

  def validateSecondaryCache(path: String, id: Int, url: String) = {
    val location = s"$path$id$extension"
    var content = ""
    val file: File = File(location)

    if (file.exists()) {
      content = String(Files.readAllBytes(Paths.get(file.getPath)))
    } else {
      val out = new PrintWriter(location)
      content = Source.fromURL(url).mkString
      out.print(content)
      out.flush()
    }
    content
  }

}
