import QtQuick 2.2
import QtQuick.Controls 1.1
import QtTest 1.0

import "../my-qml-lib"
import "../external"

MyQmlType {
  id: testObj

  TestCase {
    name: "QML Test Sample"

    property Deps deps
    property var depsFactory: Qt.createComponent("Deps.qml")

    property QmlUtil util
    property var utilFactory: Qt.createComponent("../external/QmlUtil.qml")

    SignalSpy {
      id: depsCreatedSpy
      target: deps
      signalName: "created"
    }

    function setup() {
      if (deps != null) {
        teardown()
      }
      verify(depsFactory != null)
      compare(depsFactory.status, Component.Ready)
      deps = depsFactory.createObject(this)
    }

    function teardown() {
      if (deps != null) {
        deps.destroy()
        deps = null
      }
    }

    function test_should_do_something() {
      setup()
      // test code (assertion......)

      compare(testObj.number, 17)
      teardown()
    }
  }
}
