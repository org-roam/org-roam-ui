import * as React from 'react'
import {
  StyleProp,
  TextStyle,
  TouchableOpacity,
  View,
  ViewStyle,
} from 'react-native'
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

export interface LocalProps {
  /**
   * An optional style override useful for padding & margin.
   */
  style?: StyleProp<ViewStyle>
}

/**
 * Describe your component here
 */
export const LocalButton = observer(function LocalButton(
  props: LocalProps,
): boolean {
  const { style } = props
  const styles = flatten([CONTAINER, style])

  return (
    <Icon
      color="#a99f1f"
      name="graph"
      style={{ position: 'absolute', zIndex: 100, width: 500 }}
    />
  )
})
