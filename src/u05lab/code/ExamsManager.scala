package u05lab.code

object ExamsManager extends App {

  sealed trait Kind
  object Kind {
    case object FAILED extends Kind {
      override def toString: String = "FAILED"
    }
    case object RETIRED extends Kind{
      override def toString: String = "RETIRED"
    }
    case object SUCCEEDED extends Kind{
      override def toString: String = "SUCCEEDED"
    }
  }

  sealed trait ExamResult {
    def kind: Kind
    def evaluation: Option[Int]
    def laude: Boolean
  }

  object ExamResult {
    def failed: ExamResult = ExamResultImpl(Kind.FAILED)
    def retired: ExamResult = ExamResultImpl(Kind.RETIRED)
    def succeededCumLaude: ExamResult = ExamResultImpl(Kind.SUCCEEDED, Some(30), laude = true)
    def succeeded(grade: Int, laude: Boolean = false): ExamResult =
      ExamResultImpl(Kind.SUCCEEDED, Some(grade), laude)

    private case class ExamResultImpl(kind: Kind,
                                      evaluation: Option[Int] = None,
                                      laude: Boolean = false) extends ExamResult
  }

  trait ExamsManager {
    def createNewCall(call: String): Unit

    def addStudentResult(call: String, student: String, result: ExamResult): Unit

    def getAllStudentsFromCall(call: String): Set[String];

    def getEvaluationsMapFromCall(call: String): Map[String, Int]

    def getResultsMapFromStudent(student: String): Map[String, String]

    def getBestResultFromStudent(student: String): Option[Int]
  }

  object ExamsManager {
    def apply(): ExamsManager = new ExamsManagerImpl

    class ExamsManagerImpl extends ExamsManager {
      private var calls: Map[String, Map[String, ExamResult]] = Map()

      override def createNewCall(call: String): Unit = {
        require(call != "", "Call cannot be empty")
        require(!(calls contains call), s"Call $call already added")
        calls += (call -> Map())
      }

      override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
        require(call != "" || student != "" || result != null, "Parameters cannot be empty")
        require(calls contains call, s"$call not exists!")
        require(!(calls(call) contains student), s"student $student already has a result for call $call")
        calls = calls + (call -> (calls(call) + (student -> result)))
      }

      override def getAllStudentsFromCall(call: String): Set[String] = {
        require(call != "", "Call cannot be empty")
        require(calls contains call, s"$call not exists!")
        calls(call).keySet
      }

      override def getEvaluationsMapFromCall(call: String): Map[String, Int] = {
        require(call != "", "Call cannot be empty")
        require(calls contains call, s"$call not exists!")
        calls(call).collect {
          case (student, result) if result.kind == Kind.SUCCEEDED => student -> result.evaluation.get
        }
      }

      override def getResultsMapFromStudent(student: String): Map[String, String] = {
        require(student != "", "Student cannot be empty")
        calls collect {
          case (call, results) if results contains student => call -> calls(call)(student).toString
        }
      }

      override def getBestResultFromStudent(student: String): Option[Int] = {
        require(student != "", "Student cannot be empty")
        calls.collect {
          case (_, results) if results.contains(student) => results(student).evaluation
        }.toList.sortBy(o => o.getOrElse(0))((a,b) => b-a).head
      }
    }
  }
}