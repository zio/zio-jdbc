package zio.jdbc

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object ZStatefulConnectionSpec extends ZIOSpecDefault {

  val statefulConnectionLayer: TaskLayer[ZStatefulConnection] =
    ZLayer.fromZIO(ZStatefulConnection.make(new TestConnection))

  val poolConfig: ZConnectionPoolConfig =
    ZConnectionPoolConfig.default.copy(minConnections = 1, maxConnections = 1)

  val connectionPoolLayer: TaskLayer[ZConnectionPool] =
    ZConnectionPool.h2test(poolConfig)

  def isAutoCommitDirty(dirtyBits: Int): Boolean =
    (dirtyBits & ZStatefulConnection.DirtyBitAutoCommit) != 0

  def getAutoCommit(stateful: ZStatefulConnection): Task[Boolean] =
    ZIO.attemptBlocking(stateful.underlying.getAutoCommit)

  val initSpec =
    suite("initialization")(
      test("make") {
        for {
          stateful  <- ZIO.service[ZStatefulConnection]
          dirtyBits <- stateful.dirtyBits.get
        } yield assert(dirtyBits)(equalTo(ZStatefulConnection.DirtyBitInitial)) &&
          assert(stateful.defaultIsolationLevel)(equalTo(TransactionIsolationLevel.ReadUncommitted))
      }
    ).provide(statefulConnectionLayer)

  val integrationSpec =
    suite("integration")(
      test("setAutoCommit(false) call makes bits dirty") {
        transaction {
          for {
            stateful  <- ZIO.service[ZConnection].map(_.stateful)
            _         <- stateful.setAutoCommit(false)
            dirtyBits <- stateful.dirtyBits.get
            state     <- stateful.state.get
          } yield assertTrue(isAutoCommitDirty(dirtyBits)) &&
            assert(state.autoCommitMode)(equalTo(false))
        }
      },
      test("calling setAutoCommit(false) multiple times makes bits dirty") {
        transaction {
          for {
            stateful  <- ZIO.service[ZConnection].map(_.stateful)
            times     <- Random.nextIntBetween(2, 10)
            _         <- stateful.setAutoCommit(false).repeatN(times)
            dirtyBits <- stateful.dirtyBits.get
            state     <- stateful.state.get
          } yield assertTrue(isAutoCommitDirty(dirtyBits)) &&
            assert(state.autoCommitMode)(equalTo(false))
        }
      },
      test("setAutoCommit(true) call doesn't make bits dirty") {
        transaction {
          for {
            stateful  <- ZIO.service[ZConnection].map(_.stateful)
            _         <- stateful.setAutoCommit(true)
            dirtyBits <- stateful.dirtyBits.get
            state     <- stateful.state.get
          } yield assert(isAutoCommitDirty(dirtyBits))(equalTo(false)) &&
            assertTrue(state.autoCommitMode)
        }
      },
      test("resetState successfully") {
        transaction {
          for {
            stateful         <- ZIO.service[ZConnection].map(_.stateful)
            _                <- stateful.setAutoCommit(false)
            autoCommitBefore <- getAutoCommit(stateful)
            dirtyBitsBefore  <- stateful.dirtyBits.get
            stateBefore      <- stateful.state.get
            _                <- stateful.resetState
            autoCommitAfter  <- getAutoCommit(stateful)
            dirtyBitsAfter   <- stateful.dirtyBits.get
            stateAfter       <- stateful.state.get
          } yield assert(autoCommitBefore)(equalTo(false)) &&
            assertTrue(isAutoCommitDirty(dirtyBitsBefore)) &&
            assert(stateBefore.autoCommitMode)(equalTo(false)) &&
            assertTrue(autoCommitAfter) &&
            assert(isAutoCommitDirty(dirtyBitsAfter))(equalTo(false)) &&
            assertTrue(stateAfter.autoCommitMode)
        }
      }
    ).provide(connectionPoolLayer)

  override def spec: Spec[TestEnvironment, Any] =
    suite("StatefulConnectionSpec")(
      initSpec,
      integrationSpec
    ) @@ sequential

}
