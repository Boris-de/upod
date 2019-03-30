package mobi.upod.app.gui.preference

import android.app.Activity
import de.wcht.upod.R
import mobi.upod.android.view.wizard.{ValueChoice, _}
import mobi.upod.app.AppInjection
import mobi.upod.app.gui.{MainActivity, UsageTips}
import mobi.upod.app.gui.preference.importing.{DiagnosticsImport, OPMLImport}
import mobi.upod.app.storage._

final class StartupWizardActivity extends WizardActivity with StoragePermissionRequestActivity with AppInjection with UsageTips {

  import mobi.upod.app.gui.preference.StartupWizardActivity._

  override def getActivity: Activity = this

  override protected def hasNextPage(currentPageIndex: Int, currentPageKey: String): Boolean = currentPageKey match {
    case PageKeyWelcome | PageKeyMigratingUser | PageKeyOPMLImportInformation => true
    case _ => false
  }

  override protected def createFirstPage: WizardPage =
    new WelcomePage

  override protected def createNextPage(currentPageIndex: Int, currentPageKey: String): WizardPage = currentPageKey match {
    case PageKeyWelcome => userType match {
      case Some(NewUser) => new NewUserPage
      case Some(OPMLMigratingUser) => new OPMLImportInstructions(this)
      case Some(MigratingUser) => new MigratingUserPage(this)
      case _ => new RecurringUserPage
    }
    case PageKeyMigratingUser =>
      if (ensureSingleChoiceBoolean(isMigratingUser)) {
        new DiagnosticsImport(PageKeyDiagnosticsImport)
      } else {
        new OPMLImportInstructions(this)
      }
    case PageKeyOPMLImportInformation =>
      if (ensureSingleChoiceBoolean(isImportOPMLNow)) {
        new OPMLImport(PageKeyOPMLImport)
      } else {
        new WelcomePage
      }
    case _ => new RecurringUserPage
  }

  override protected def onFinishButtonClicked(): Unit = {
    if (isRecurringUser) {
      disableAllTips()
    }
    finishWizardOnStorageAvailable = true
    ensureExternalStoragePermission(true)
  }

  private def isRecurringUser = {
    val key = currentPage.key
    key == PageKeyRecurringUser || key == PageKeyMigratingUser
  }

  protected def onFinish(): Unit = {
    val preferences = inject[InternalAppPreferences]
    preferences.showStartupWizard := false
  }

  override protected val followUpActivity =
    classOf[MainActivity]

  override protected val requestPermissionOnStart: Boolean = false

  override protected def shouldRequestExternalStoragePermission: Boolean = true

  override protected[preference] def onStorageAvailable(): Unit = {
    super.onStorageAvailable()
    if (finishWizardOnStorageAvailable) {
      finishWizard()
    }
  }

  private def ensureSingleChoiceBoolean(maybeBoolean: Option[Boolean]) = maybeBoolean match {
    case Some(b) => b
    case None => throw new IllegalStateException("Cannot happen with SimpleSingleChoicePage[Boolean]")
  }
}

object StartupWizardActivity {
  private val PageKeyWelcome = "welcome"
  private val PageKeyNewUser = "newUser"
  private val PageKeyRecurringUser = "recurringUser"
  private val PageKeyMigratingUser = "migratingUser"
  private val PageKeyOPMLImport = "OPMLImport"
  private val PageKeyOPMLImportInformation = "OPMLImportInformation"
  private val PageKeyDiagnosticsImport = "DiagnosticsImport"

  private val NewUser = 0
  private val ExistingUser = NewUser + 1
  private val MigratingUser = ExistingUser + 1
  private val OPMLMigratingUser = MigratingUser + 1

  private var userType: Option[Int] = None
  private var isMigratingUser: Option[Boolean] = None
  private var isImportOPMLNow: Option[Boolean] = None

  private var finishWizardOnStorageAvailable = false

  def start(activity: Activity): Unit = {
    WizardActivity.start(activity, classOf[StartupWizardActivity])
  }

  def startInsteadOf(activity: Activity): Unit = {
    activity.finish()
    start(activity)
  }

  class WelcomePage extends SimpleSingleChoicePage[Int](
    PageKeyWelcome,
    R.string.wizard_welcome,
    R.string.wizard_welcome_introduction,
    0,
    userType,
    choice => userType = Some(choice),
    ValueChoice(NewUser, R.string.wizard_welcome_option_new_user),
    ValueChoice(ExistingUser, R.string.wizard_welcome_option_existing_user),
    ValueChoice(OPMLMigratingUser, R.string.wizard_welcome_option_opml),
    ValueChoice(MigratingUser, R.string.wizard_welcome_option_migrating)
  )

  class NewUserPage extends WizardWebPage(PageKeyNewUser, R.string.wizard_new_user, R.string.wizard_new_user_introduction)

  class RecurringUserPage extends WizardWebPage(PageKeyRecurringUser, R.string.wizard_recurring_user, R.string.wizard_recurring_user_introduction)

  class MigratingUserPage(val parent: StartupWizardActivity) extends SimpleSingleChoicePage[Boolean](
    PageKeyMigratingUser,
    R.string.wizard_migration,
    R.string.wizard_migration_introduction,
    0,
    None,
    choice => isMigratingUser = Some(choice),
    ValueChoice(false, R.string.wizard_migration_import_opml),
    ValueChoice(true, R.string.wizard_migration_import_diagnostics)
  ) {
    override protected def onChoiceChanged(choice: Boolean): Unit = {
      super.onChoiceChanged(choice)
      // the user made a choice, as both choices require storage access, ask for it now
      parent.ensureExternalStoragePermission(true)
    }
  }

  class OPMLImportInstructions(val parent: StartupWizardActivity) extends SimpleSingleChoicePage[Boolean](
    PageKeyOPMLImportInformation,
    R.string.wizard_migration_import_opml,
    R.string.wizard_migration_import_opml_introduction,
    0,
    None,
    choice => isImportOPMLNow = Some(choice),
    ValueChoice(true, R.string.wizard_migration_import_opml),
    ValueChoice(false, R.string.wizard_back)
  ) {
    override protected def onChoiceChanged(choice: Boolean): Unit = {
      super.onChoiceChanged(choice)
      if (choice) {
        parent.ensureExternalStoragePermission(true)
      }
    }
  }
}
