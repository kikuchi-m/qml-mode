import QtQuick 2.2
import QtQuick.Controls 1.1
import QtTest 1.0

TestCase {
  name: "QML Test Sample"

  property MyQmlType testObj
  property var testObjFactory: Qt.createComponent("MyQmlType.qml")

  SignalSpy {
    id: testObjMySignalSpy
    target: testObj
    signalName: "mySignal"
  }

    function setup() {
    if (testObj != null) {
      teardown();
    }
    testObj = testObjFactory.createObject(this)
  }

  function teardown() {
    if (testObj != null) {
      testObj.destroy()
      testObj = null
    }
  }

  function test_should_do_something() {
    setup()
    // test code (assertion......)
    teardown()
  }
}
