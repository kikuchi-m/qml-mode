import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 2.0 as L

// insrted tabs
import   	  	 Foo.Bar.Nee 12.345

import "../common/qml"
import "../custom/qml" as Custom
import "../private/qml" as private_1

import "../script/util.js" as Utility_Scripts

Item {
  id: item1

  Layout.fillWidth: parent
  longlonglonglonglonglonglonglonglonglongNameProperty: value

  property int         prop1 : 123
  property bool        prop2: true
  property real        prop3: 456.789
  property double      prop4: 0.12345
  property string      prop5: "string property"
  property url         prop6: "~/path/to/file"
  property list<Item>  prop7: [
    Item { },
    Item { },
    Item { }
  ]

  property color prop_color
  property font prop_font
  property matrix4x4 prop_matrix4x4
  property quaternion prop_quaternion
  property vector2d prop_vector2d
  property vector3d prop_vector3d
  property vector4d prop_vector4d
  property date prop_date
  property point prop_point
  property size prop_size
  property rect prop_rect

  property My.QmlType myType: My.QmlType { }

  property Type invalidPropDef:
  property Type invalidPropDef:
  property Type invalidPropDef: \\
  property Type invalidPropDef: .
  property Type invalidPropDef: =
  property Type invalidPropDef: ;
  property Type invalidPropDef: :
  property Type invalidPropDef: #
  property Type invalidPropDef: $
  property Type invalidPropDef: |
  property Type invalidPropDef: &
  property Type invalidPropDef: ^
  property Type invalidPropDef: /
  property Type invalidPropDef: *
  property Type invalidPropDef: +
  property Type invalidPropDef: -

  readonly property var      prop8: function() {
    return "this is a function property";
  }


  // function foo () {
  // }

  // comment (contains parenthesis)
  Tex {
    text: "text (parenthesis)"
  }

  property alias binded: child.properyName

  default    property    int   defaultProp
  readonly   property    bool  readonlyProp: true
  readonly   property    real  nonInit

  signal notifySignal()

  function execSomething(param1, param2) {
    var x, y, z;
  }

  Entity  { id: a1; property string a1Str: "a1";property int number: 87654 }

  Under_Score {
    id: unsco
    under_score: "underscore"
  }

  // line commnet ......
  // ......
  // ......

  Rectangle {
    // aaaa    aA A  */ Item { }
    /*     a*/  Item_after_comment  { }
    /*
    multi
    line
    comment
    */ Item {
      a:b
      items:[]
    }

    // Item {
    //   Text {
    //     text: "line commont"
    //   }
    // }

    /*Item {
      Text {
        text: "block commont"
        // ......
        // ......
      }
    }*/

    /*
    Item {
      Text {
        text: "block commont"
      }
    }
    */

    Custom.ErrorText {
      property bool hasError: false
      property list<Custom.Entity> entityList: [
        Custom.Entity { value: v1 }
        Custom.Entity { value: v2 }
        Custom.Entity { value: v3 }
      ]
    }

    // invalid declaration
    Custom ErrorText {
      property bool hasError: false
      property Custom.ErrorContent content: Custom.ErrorContent {

      }
    }

    Rectangle
    {
      x		: 120 // insrted tabs
      y  : 320

      border {
        width: 2
        color    :    "#ef4"
      }

      Text {
        function fun1() {
          var x = (a + (b - (c * (d / (e + f)))));
          var y = a ^ (b ^ c);
          return a && (b || (c && d));
        }

        Component.onCompleted: {
          var a = 10;
          var b = 20;
          console.log(a + b);
        }
      }
    }

    Rectangle
    {
      border.width: 2
      border.color    :    "#ef4"
    }

    Rectangle { border.width: 0; boder.color: "SteelBlue" }

    SingleLineDef { property real realValue: 1234567890 }

    Text { id:text2; width: 200; hitght: 23  ;  text  :  "inline" }
  }

  TextField {
    validator: IntValidator {
      bottom: -273
      top: 2349
    }
  }

  TextField {
    validator: RegExpValidator { regexp: "^[a-z_-\.]+$" }
  }
}
