package timeusage

import org.apache.spark.sql.{Column, ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite, ShouldMatchers}

import scala.util.Random

class TimeUsageSuite extends FunSuite with BeforeAndAfterAll with ShouldMatchers {
  test("the classifiedColumns method works") {
    import timeusage.TimeUsage.{classifiedColumns, prefixLeisureActivities, prefixPrimaryNeeds, prefixWorkingActivities}

    import Random.shuffle

    Random.setSeed(System.currentTimeMillis())

    def suffix = Random.alphanumeric.take(15).mkString

    // Create a fake column name with each prefix.
    val p = shuffle(for (p <- prefixPrimaryNeeds) yield s"${p}$suffix")
    val w = shuffle(for (p <- prefixWorkingActivities) yield s"${p}$suffix")
    val l = shuffle(for (p <- prefixLeisureActivities) yield s"${p}$suffix")
    val testColumnNames = shuffle(p ++ w ++ l) toList

    val (primaryNeeds, workingActivities, leisureActivities) = classifiedColumns(testColumnNames)

    // Each column name must start with one of the prefixes.
    def columnsPrefixed(columns: List[Column], prefixes: Set[String]) = columns foreach { column =>
      prefixes.find(column.toString.startsWith(_)) should not be empty
    }

    columnsPrefixed(primaryNeeds, prefixPrimaryNeeds)
    columnsPrefixed(workingActivities, prefixWorkingActivities)
    columnsPrefixed(leisureActivities, prefixLeisureActivities)

    primaryNeeds.length + workingActivities.length + leisureActivities.length shouldBe testColumnNames.length
  }

}
