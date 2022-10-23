package scalix

object MovieObjects {
  case class People(results: List[Person])

  case class Person(id: Int)

  case class Movies(cast: Set[Movie])

  case class Movie(id: Int, title: String)

  case class Workers(crew: List[Worker])

  case class Worker(id: Int, name: String, job: String)

  case class FullName(name: String, surname: String)

}
