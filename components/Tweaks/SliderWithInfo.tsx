import {
  Text,
  Box,
  Slider,
  SliderFilledTrack,
  SliderThumb,
  SliderTrack,
  Tooltip,
} from '@chakra-ui/react'
import React, { useContext } from 'react'
import { ThemeContext } from '../../util/themecontext'
import { InfoTooltip } from './InfoTooltip'

export interface SliderWithInfoProps {
  min?: number
  max?: number
  step?: number
  value: number
  onChange: (arg0: number) => void
  label: string
  infoText?: string
}
export const SliderWithInfo = ({
  min = 0,
  max = 10,
  step = 0.1,
  value = 1,
  ...rest
}: SliderWithInfoProps) => {
  const { onChange, label, infoText } = rest
  const { highlightColor } = useContext(ThemeContext)
  return (
    <Box key={label}>
      <Box display="flex" alignItems="flex-end">
        <Text>{label}</Text>
        {infoText && <InfoTooltip infoText={infoText} />}
      </Box>
      <Slider value={value} onChange={onChange} min={min} max={max} step={step}>
        <SliderTrack>
          <SliderFilledTrack />
        </SliderTrack>
        <Tooltip bg={highlightColor} label={value.toFixed(1)}>
          <SliderThumb bg="white" />
        </Tooltip>
      </Slider>
    </Box>
  )
}
