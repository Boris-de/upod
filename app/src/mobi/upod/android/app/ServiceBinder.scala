package mobi.upod.android.app

import android.annotation.TargetApi
import android.content.{Context, Intent, ServiceConnection}
import mobi.upod.android.util.ApiLevel

import scala.reflect.ClassTag

class ServiceBinder[A, B <: BoundService[A]](implicit serviceTag: ClassTag[B]) {

  private def createIntent(context: Context) =
    new Intent(context, serviceTag.runtimeClass)

  def bind(context: Context, connection: ServiceConnection): Unit = {
    val intent = createIntent(context)
    starter(context)(intent)
    context.bindService(intent, connection, Context.BIND_AUTO_CREATE)
  }

  @TargetApi(ApiLevel.Oreo)
  private def starter(context: Context): Intent => Unit = {
    if (ApiLevel >= ApiLevel.Oreo) {
      context.startForegroundService
    } else {
      context.startService
    }
  }

  def unbind(context: Context, connection: ServiceConnection): Unit = {
    context.unbindService(connection)
    context.stopService(createIntent(context))
  }
}