import java.util.concurrent._
import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, StdIn}
import java.io.{File, PrintWriter}
import scala.util.Random

// Утилита для работы с таймером бездействия
class InactivityTimer(timeout: Long, unit: TimeUnit, lockAction: () => Unit) {
  private val scheduler = Executors.newScheduledThreadPool(1)
  private var future: ScheduledFuture[_] = _

  def reset(): Unit = {
    if (future != null) future.cancel(false)
    future = scheduler.schedule(new Runnable {
      def run(): Unit = lockAction()
    }, timeout, unit)
  }

  def stop(): Unit = {
    scheduler.shutdownNow()
  }
}

// Основной объект приложения
object KeyboardBiometricsAuth {

  private val inactivityTimer = new InactivityTimer(1, TimeUnit.MINUTES, () => {
    println("\nВремя бездействия истекло. Пожалуйста, повторите аутентификацию.")
    main(Array.empty) // Рестарт приложения для аутентификации
  })

  // Функция для измерения времени ввода фразы
  def measureTypingSpeed(phrase: String): ArrayBuffer[Long] = {
    println(s"Please type the following phrase: $phrase")
    val start = System.currentTimeMillis()
    val input = scala.io.StdIn.readLine()
    val end = System.currentTimeMillis()

    if (input == phrase) {
      val totalTime = end - start
      println(s"Time taken: $totalTime milliseconds")
      ArrayBuffer(totalTime) // Просто возвращаем общее время ввода для упрощения
    } else {
      println("Error in typing. Start again.")
      measureTypingSpeed(phrase) // Перезапуск измерения для фразы
    }
  }

  // Чтение и дешифровка фраз из файла
  def readEncryptedPhrases(filename: String): List[String] = {
    val shift = 3 // Простое шифрование Цезаря
    Source.fromFile(filename).getLines.toList.map { encryptedPhrase =>
      encryptedPhrase.map { char =>
        if (char.isLetter) {
          val base = if (char.isUpper) 'A' else 'a'
          ((char - base + 26 - shift) % 26 + base).toChar
        } else {
          char
        }
      }
    }
  }

  // Регистрация пользователя
  def registerUser(phrasesFile: String, usersFile: String): Unit = {
    val phrases = readEncryptedPhrases(phrasesFile)
    val phrase = phrases(Random.nextInt(phrases.length))
    val timings = ArrayBuffer[ArrayBuffer[Long]]()
    for (_ <- 1 to 4) {
      timings += measureTypingSpeed(phrase)
    }
    val avgTimings = timings.transpose.map(t => t.sum / t.length) // Вычисление среднего времени для каждой пары клавиш
    val userRecord = s"$phrase,${avgTimings.mkString(",")}\n"
    val pw = new PrintWriter(new File(usersFile), "UTF-8")
    try {
      pw.append(userRecord) // Добавление записи пользователя в файл
    } finally {
      pw.close()
    }
  }

  // Аутентификация пользователя
  def authenticateUser(phrase: String, usersFile: String): Boolean = {
    val userRecords = Source.fromFile(usersFile).getLines()
    val userTimings = userRecords.find(_.startsWith(phrase)).map(_.split(",").tail.map(_.toLong))
    userTimings match {
      case Some(savedTimings) =>
        val timings = measureTypingSpeed(phrase)
        val average = timings.sum / timings.length
        val savedAverage = savedTimings.sum / savedTimings.length
        Math.abs(average - savedAverage) < 1000 // Допустимая разница в миллисекундах
      case None =>
        false
    }
  }

  // Основная функция программы
  def main(args: Array[String]): Unit = {
    inactivityTimer.reset() // Активация таймера бездействия

    println("Welcome to Keyboard Biometrics Authentication System")
    println("Choose an option: [1] Register [2] Authenticate")
    val option = StdIn.readInt()

    option match {
      case 1 =>
        println("Registering a new user.")
        val phrasesFile = "C:\\Users\\tammm\\Desktop\\phrases.txt" // Укажите путь к вашему файлу с фразами
        val usersFile = "C:\\Users\\tammm\\Desktop\\users.txt"    // Укажите путь к вашему файлу с пользователями
        registerUser(phrasesFile, usersFile)
      case 2 =>
        println("Authenticating an existing user.")
        val usersFile = "C:\\Users\\tammm\\Desktop\\users.txt" // Укажите путь к вашему файлу с пользователями
        println("Enter your phrase:")
        val phrase = StdIn.readLine()
        if (authenticateUser(phrase, usersFile)) {
          println("Authentication successful.")
          inactivityTimer.reset()
        } else {
          println("Authentication failed.")
        }
      case _ =>
        println("Invalid option.")
    }

    inactivityTimer.stop() // Остановка таймера при завершении работы программы
  }
}
