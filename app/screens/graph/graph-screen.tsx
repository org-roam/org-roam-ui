import React, { useEffect, useState } from "react"
import { observer } from "mobx-react-lite"
import { TouchableOpacity, View, ViewStyle } from "react-native"
import { Screen, Text } from "../../components"
// import { useNavigation } from "@react-navigation/native"
// import { useStores } from "../../models"
import { color } from "../../theme"

import { Graph } from "../../components"
import { Tweaks } from "../../components"

import genRandomTree from "../../data/randomdata"

import AsyncStorage from "@react-native-async-storage/async-storage"

import axios from "axios"

import rando from "../../data/rando.json"


const ROOT: ViewStyle = {
  backgroundColor: color.palette.black,
  flex: 1,
}

import { LocalButton } from "../../components/"
import { GraphUi } from "../../components/graph-ui/graph-ui"
import { Switch } from "react-native-elements"
export const GraphScreen = observer(function GraphScreen() {
  // Pull in one of our MST stores
  // const { someStore, anotherStore } = useStores()

  // Pull in navigation via hook
  // const navigation = useNavigation()

  const [physics, setPhysics] = useState({})
  const [graphData, setGraphData] = useState()
  const [nodeIds, setNodeIds] = useState([])
  //  { "nodes": [{ "id": 1 }, { "id": 2 }], "links": [{ "target": 1, "source": 2 }] });
  const physicsInit = {
    charge: -350,
    collision: true,
    linkStrength: 0.1,
    linkIts: 1,
    particles: 2,
    linkOpacity: 0.4,
    linkWidth: 1,
    particleWidth: 4,
    nodeRel: 4,
    labels: true,
    labelScale: 1.5,
    alphaDecay: 0.16,
    alphaTarget: 0,
    velocityDecay: 0.25,
    gravity: 0.5,
    gravityOn: true,
    hover: true,
    colorful: true,
    galaxy: true,
    rootId: 0,
  }

  const getData = async () => {
    try {
      const value: string = await AsyncStorage.getItem("@physics")
      if (value !== null) {
        const valueJson = JSON.parse(value)
        if (Object.keys(valueJson).length === Object.keys(physicsInit).length) {
          valueJson.local=false;
          return valueJson
        } else {
          return physicsInit
        }
      } else {
        return physicsInit
      }
    } catch (e) {
      console.log(e)
    }
  }
  const storeData = async (value) => {
    try {
      let jsonVal = JSON.stringify(value)
      console.log(jsonVal + " " + value)
      await AsyncStorage.setItem("@physics", jsonVal)
    } catch (e) {
      console.log(e)
    }
  }

  // hook to save the current configuration of the physics tweaks
  // after it is updated
  useEffect(() => {
    if (timer) {
      clearTimeout(timer)
    }
    // set timer so the thing doesn't run every single slider tick
    const timer = setTimeout(() => {
      storeData(physics)
    }, 1000)
    return () => clearTimeout(timer)
  }, [physics])

  //"ComponentOnMount"
  // Get previous settings and the data from the org-roam-server
  const sanitizeGraph = (data) => {
    const cleanLinks = []
    data.links.forEach((link, j) => {
      let target
      let source
      for (let i = 0; i < data.nodes.length; i++) {
        let a = data.nodes[i]
        !a.neighbors && (a.neighbors = [])
        !a.links && (a.links = [])
        // the first time around,
        // index the node as not a part of the local graph
          !j && (a.local=false);
        if (link.target === data.nodes[i].id) {
          a.links.push(link)
          target = [a, i]
          link.target === link.source ? null : cleanLinks.push(link)
        } else if (link.source === data.nodes[i].id) {
          a.links.push(link)
          source = [a, i]
        }
      }
      if (target && source) {
        data.nodes[target[1]].neighbors.push(source[0])
        data.nodes[source[1]].neighbors.push(target[0])
        link.sourceIndex = source[1]
        link.targetIndex = target[1]
        link.index = [j]
        link.local = false;
      }
    })
    data.links = cleanLinks;
    return data;
  }


const [theme, setTheme] = useState({
base1: "#1c1f24",
base2: "#21272d",
base3: "#23272e",
base4: "#484854",
base5: "#62686E",
base6: "#757B80",
base7: "#9ca0a4",
base8: "#DFDFDF",
bg: "#242730",
"bg-alt": "#2a2e38",
blue: "#51afef",
cyan: "#5cEfFF",
"dark-blue": "#1f5582",
"dark-cyan": "#6A8FBF",
fg: "#bbc2cf",
"fg-alt": "#5D656B",
green: "#7bc275",
grey: "#484854",
magenta: "#C57BDB",
orange: "#e69055",
red: "#ff665c",
teal: "#4db5bd",
violet: "#a991f1",
yellow: "#FCCE7B",
    });

  useEffect(() => {
    getData().then((data) => setPhysics(data))
    axios.get("http://localhost:35901/theme")
         .then((theme)=>{setTheme(theme.data);
             console.log(theme.data)})
      .catch((e) => {
        console.log("No theme found")
      })
    axios
      .get("http://localhost:35901/graph")
      .then((dataa) => {
        let cleanData = sanitizeGraph(dataa.data)
        console.log(cleanData)
        setGraphData(cleanData)
      })
      .catch((e) => {
        console.log(e)
        console.log("Couldn't get data.")
        //setGraphData(rando);
      })
  }, []);

  const [threeDim, setThreeDim] = useState(false);
    const [local, setLocal] = useState(false);

  if (!graphData) {
    return null
  } else {
    return (
      <Screen style={ROOT} preset="fixed">
        <View style={{display: "flex", flexDirection: "row", height: "100%", width: "100%",position: "absolute", zIndex:150}}>
        <Tweaks physics={physics} setPhysics={setPhysics} />
        <View style={{marginLeft: "auto", marginRight:"2%", marginTop: "2%", zIndex: 5, position: "relative", height: "15%" }}>
        <LocalButton  local={local} setLocal={setLocal} />
        <View style={{display: "flex", flexDirection: "row", alignItems: "center",justifyContent: "center", paddingTop: "2%"}}>
        <Text preset="header" text="3D" style={{marginLeft: 10}} />
                    <Switch
            color="#a991f1"
            trackColor={{
                false: "#62686E",
                true: "#a991f1"
            }}
            style={{
    height: 25,
                        marginVertical: 10,
            marginLeft: 20}}
            value={threeDim}
            onValueChange={() => {
              setThreeDim(!threeDim)
            }}
          />
        </View>
        </View>
        <Graph
        style={{position: "absolute"}}
            setPhysics={setPhysics} physics={physics} gData={graphData} nodeIds={nodeIds}
        threeDim={threeDim} setThreeDim={setThreeDim}
        local={local} setLocal={setLocal}/>
        </View>
      </Screen>
    )
  }
})
