import React, { useEffect, useState } from "react"
import { observer } from "mobx-react-lite"
import { ViewStyle } from "react-native"
import { Screen, Text } from "../../components"
// import { useNavigation } from "@react-navigation/native"
// import { useStores } from "../../models"
import { color } from "../../theme"

import { Graph } from "../../components"
import { Tweaks } from "../../components"

import genRandomTree from "../../data/randomdata"

import AsyncStorage from "@react-native-async-storage/async-storage"

import axios from "axios";

const ROOT: ViewStyle = {
  backgroundColor: color.palette.black,
  flex: 1,
}

export const GraphScreen = observer(function GraphScreen() {
  // Pull in one of our MST stores
  // const { someStore, anotherStore } = useStores()

  // Pull in navigation via hook
  // const navigation = useNavigation()

  const [physics, setPhysics] = useState({})
  const physicsInit = {
    charge: -30,
    collision: false,
    linkStrength: 1,
    linkIts: 1,
    collapse: false,
    threedim: false,
    particles: 2,
    linkOpacity: 1,
    linkWidth: 1,
    particleWidth: 1,
    nodeRel: 1,
  }

  const getData = async () => {
    try {
      const value: string = await AsyncStorage.getItem("@physics");
      if (value !== null ) {
          const valueJson = JSON.parse(value);
              if ( Object.keys(valueJson).length === Object.keys(physicsInit).length) {
        return valueJson;
        } else { return physicsInit };
      } else {
        return physicsInit
      }
    } catch (e) {
      console.log(e)
    }
  }

  //"ComponentOnMount"
  // Get previous settings and the data from the org-roam-server
  useEffect(() => {
    getData().then((data) => setPhysics(data));
    //  axios.get('/roamData')
    //       .then(()=>console.log("Whoo got data"))
    //.catch((e)=>{
    //    console.log(e);
    //    console.log("Couldn't get data.");
    //});
  }, [])

  const storeData = async (value) => {
    try {
      const jsonValue = JSON.stringify(value)
      await AsyncStorage.mergeItem("@physics", jsonValue)
      console.log("Writing " + jsonValue)
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
      const test = getData()
      console.log(test)
    }, 1000)
    return () => clearTimeout(timer)
  }, [physics])

  const gData = genRandomTree()

  return (
    <Screen style={ROOT} preset="scroll">
      <Tweaks physics={physics} setPhysics={setPhysics} />
      <Graph physics={physics} gData={gData} />
    </Screen>
  )
})
