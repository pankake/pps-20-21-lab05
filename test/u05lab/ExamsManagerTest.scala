package u05lab

import org.junit.jupiter.api.Assertions.{assertEquals, assertThrows}
import org.junit.jupiter.api.Test
import u05lab.code.ExamsManager.{ExamResult, ExamsManager, Kind}

class ExamsManagerTest {
  val examsManager: ExamsManager = ExamsManager()

  private def setupExams(): Unit = {
    examsManager.createNewCall("January")
    examsManager.createNewCall("February")
    examsManager.addStudentResult("January", "Commodaro", ExamResult.succeeded(30, true))
    examsManager.addStudentResult("January", "Bernardi", ExamResult.failed)
    examsManager.addStudentResult("February", "Commodaro", ExamResult.retired)
    examsManager.addStudentResult("February", "Bernardi", ExamResult.succeeded(20))
  }

  @Test def testGetAllStudentsFromCall(): Unit = {
    setupExams()
    assertEquals(Set("Commodaro", "Bernardi"), examsManager.getAllStudentsFromCall("January"))
    assertEquals(Set("Commodaro", "Bernardi"), examsManager.getAllStudentsFromCall("February"))
    assertThrows(classOf[IllegalArgumentException], () => examsManager.getAllStudentsFromCall("March"))
  }

  @Test def testGetEvaluationsMapFromCall(): Unit = {
    setupExams()
    assertEquals(Map("Commodaro" -> 30), examsManager.getEvaluationsMapFromCall("January"))
    assertEquals(Map("Bernardi" -> 20), examsManager.getEvaluationsMapFromCall("February"))
  }

  @Test def testGetResultMapFromStudent(): Unit = {
    setupExams()
    assertEquals(Map("January" -> ExamResult.succeededCumLaude.toString, "February" -> ExamResult.retired.toString),
      examsManager.getResultsMapFromStudent("Commodaro"))
    assertEquals(Map("January" -> ExamResult.failed.toString, "February" -> ExamResult.succeeded(20).toString),
      examsManager.getResultsMapFromStudent("Bernardi"))
  }

  @Test def testGetBestResultFromStudent(): Unit = {
    setupExams()
    assertEquals(Some(30), examsManager.getBestResultFromStudent("Commodaro"))
  }

}