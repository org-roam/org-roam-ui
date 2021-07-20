import * as React from 'react'
import { StyleProp, TextStyle, View, ViewStyle } from 'react-native'
import { observer } from 'mobx-react-lite'
import { color, typography } from '../../theme'
import { Text } from '../'
import { flatten } from 'ramda'
import Icon from 'react-native-vector-icons/MaterialCommunityIcons'

const CONTAINER: ViewStyle = {
  justifyContent: 'center',
}

const TEXT: TextStyle = {
  fontFamily: typography.primary,
  fontSize: 14,
  color: color.primary,
}

export interface LocalButtonProps {
  /**
   * An optional style override useful for padding & margin.
   */
  style?: StyleProp<ViewStyle>
  local
  setLocal
}

/**
 * Describe your component here
 */
export const LocalButton = observer(function LocalButton(props: LocalButtonProps) {
  const { style, local, setLocal } = props
  const styles = flatten([CONTAINER, style])

  return (
    <View style={[style, { height: 50, width: 150 }]}>
      <Icon.Button
        name={!local ? 'graph-outline' : 'graph'}
        backgroundColor="#a991f1"
        onPress={() => {
          setLocal(!local)
        }}
        size={30}
        style={{ textAlign: 'center' }}
      >
        {!local ? 'Global Graph' : 'Local Graph'}
      </Icon.Button>
    </View>
  )
})
