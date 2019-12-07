import QtQuick 2.0
import QtQuick.Controls 1.0 as Controls
import 'logic.js' as Logic

Item {
    // id
    id: root

    // property
    width: 128; height: 64
    enabled: true
    objectName: 'rootItem'
    anchors.centerIn: parent
    // property (spaced)
    opacity  :  1.0

    // property definitions
    property var value
    property int number: 10
    property bool checked: false
    property string text: 'Root Item'
    property var regex: /button/
    property var color: checked ? '#000000' : '#FFFFFF'

    // default property
    default property var textItem

    // signal
    signal pressed
    signal clicked()
    signal clickedAt(int x, int y, string text)

    // object property
    property var object: QtObject {
        objectName: "object"
        property var value
        function action() {
            console.log('action')
        }
    }

    // value source
    NumberAnimation on opacity {
        id: animation
        from: 0.0
        to: 1.0
    }
    // inline object
    Text { text: root.text }

    // qualified type
    Controls.Button { text: 'button' }

    // function
    function startAnimation(duration) {
        animation.duration = duration
        animation.start()
    }

    // signal handler
    onClickedAt: {
        if (x === 10 && y === 10) {
          console.log('clicked at x=10, y=10')
        }
    }

    // attached signal handler
    Component.onCompleted: {
        console.log('completed')
    }

    // states
    states: [
        State { name: 'default' },
        State { name: 'pressed' }
    ]
}
