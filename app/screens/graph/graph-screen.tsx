import React, { useState } from "react"
import { observer } from "mobx-react-lite"
import { ViewStyle } from "react-native"
import { Screen, Text } from "../../components"
// import { useNavigation } from "@react-navigation/native"
// import { useStores } from "../../models"
import { color } from "../../theme"

import { Graph } from "../../components"
import { Tweaks } from "../../components"

import genRandomTree from "../../data/randomdata";

const ROOT: ViewStyle = {
  backgroundColor: color.palette.black,
  flex: 1,
}

export const GraphScreen = observer(function GraphScreen() {
  // Pull in one of our MST stores
  // const { someStore, anotherStore } = useStores()

  // Pull in navigation via hook
  // const navigation = useNavigation()
  const [charge, setCharge] = useState(-30);
  const [collision, setCollision] = useState(false);
  const [linkStrength, setLinkStrength] = useState(1);
  const [linkIts, setLinkIts] = useState(1);

  const [physics, setPhysics] = useState(
  {
      charge: -30,
      collision: false,
      linkStrength: 1,
      linkIts: 1,
      collapse: false,
      threedim: false
  });

    const gData = genRandomTree();

  return (
    <Screen style={ROOT} preset="scroll">
      <Text preset="header" text="Graph" />
      <Tweaks
        physics={physics}
        setPhysics={setPhysics}
      />
      <Graph
        physics={physics}
        gData = {gData}
      />
    </Screen>
  );
});
