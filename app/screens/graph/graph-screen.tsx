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

const ROOT: ViewStyle = {
  backgroundColor: color.palette.black,
  flex: 1,
}

export const GraphScreen = observer(function GraphScreen() {
  // Pull in one of our MST stores
  // const { someStore, anotherStore } = useStores()

  // Pull in navigation via hook
  // const navigation = useNavigation()
  const [charge, setCharge] = useState(-30)
  const [collision, setCollision] = useState(false)
  const [linkStrength, setLinkStrength] = useState(1)
  const [linkIts, setLinkIts] = useState(1)

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
      const value = await AsyncStorage.getItem("@physics")
      if (value !== null || keys(value) === keys(physicsInit)) {
        return JSON.parse(value)
      } else {
        console.log(physicsInit)
        return physicsInit
      }
    } catch (e) {
      console.log(e)
    }
  }

  useEffect(() => {
    getData().then((data) => setPhysics(data))
  }, [])

  const storeData = async (value) => {
    try {
      const jsonValue = JSON.stringify(value)
      await AsyncStorage.setItem("@physics", jsonValue)
      console.log("Writing " + jsonValue)
    } catch (e) {
      console.log(e)
    }
  }
  /* const [physics, setPhysics] = useState(
   *     {
   *         charge: -30,
   *         collision: false,
   *         linkStrength: 1,
   *         linkIts: 1,
   *         collapse: false,
   *         threedim: false,
   *         particles: 2,
   *       }); */
  useEffect(() => {
    if (timer) {
      clearTimeout(timer)
      console.log("clear timer")
    }
    const timer = setTimeout(() => {
      console.log("Physics changed")
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
