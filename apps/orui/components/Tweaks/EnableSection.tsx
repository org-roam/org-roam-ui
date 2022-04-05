import { Text, Box, Collapse, Switch } from '@chakra-ui/react'
import React from 'react'
import { InfoTooltip } from './InfoTooltip'

export interface EnableSectionProps {
  label: string
  value: boolean | number
  onChange: () => void
  infoText?: string
  children: React.ReactNode
}

export const EnableSection = (props: EnableSectionProps) => {
  const { value, onChange, label, infoText, children } = props
  return (
    <Box paddingTop={2} key={label}>
      <Box display="flex" justifyContent="space-between" paddingBottom={2}>
        <Box display="flex" alignItems="center">
          <Text>{label}</Text>
          {infoText && <InfoTooltip infoText={infoText} />}
        </Box>
        <Switch isChecked={!!value} onChange={onChange} />
      </Box>
      <Collapse in={!!value} animateOpacity>
        <Box paddingLeft={4} paddingTop={2} paddingBottom={2}>
          {children}
        </Box>
      </Collapse>
    </Box>
  )
}
