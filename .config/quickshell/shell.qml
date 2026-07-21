import Quickshell // for PanelWindow
import QtQuick // for Text

PanelWindow {
  anchors {
    top: true
    left: true
    right: true
  }

  margins {
    right: 20
    left: 20
    top: 10
  }

  color: "transparent"

  implicitHeight: 32

  Rectangle {
    anchors.fill: parent
    color: "#222222"
    radius: 10
  }

  Text {
    // center the bar in its parent component (the window)
    anchors.centerIn: parent

    text: "hello world"
    color: "white"
  }
}
