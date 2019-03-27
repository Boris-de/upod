package mobi.upod.app.storage

import java.net.URL

import com.escalatesoft.subcut.inject.BindingModule
import mobi.upod.data.Mapping
import mobi.upod.util.Cursor

class ImportedSubscriptionsDao(dbHelper: DatabaseHelper)(implicit bindingModule: BindingModule)
  extends Dao[URL](ImportedSubscriptionsDao.table, dbHelper, ImportedSubscriptionsDao.mapping) {

  import ImportedSubscriptionsDao._

  override protected def columns: Map[Symbol, String] = Map(
    podcast -> PRIMARY_KEY_TEXT
  )

  override protected[storage] def upgrade(database: Database, oldVersion: Int, newVersion: Int): Unit = {
    log.info(s"upgrading imported subscriptions table from $oldVersion to $newVersion")
  }

  def add(podcasts: Seq[URL]): Unit = {
    verifyInTransaction()

    val stmt = db.compileStatement(sql"INSERT OR IGNORE INTO $table ($podcast) VALUES (?)")
    podcasts foreach { url =>
      stmt.bindString(1, url.toString)
      stmt.executeInsert()
    }
  }

  def removeNonNew(): Unit =
    execSql(sql"DELETE FROM $table WHERE $podcast IN (SELECT url FROM podcast WHERE subscribed=1)")

  def removeAll(): Unit =
    execSql(sql"DELETE FROM $table")

  def list: Cursor[URL] =
    findMultiple(sql"SELECT * FROM $table")
}

object ImportedSubscriptionsDao extends DaoObject {
  override val table = Table('importedSubscriptions)
  val podcast = 'podcast

  private val mapping = Mapping.simpleMapping("podcast" -> Mapping.url)
}