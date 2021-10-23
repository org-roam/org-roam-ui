import { ChevronDownIcon } from '@chakra-ui/icons'
import {
  Button,
  Flex,
  Menu,
  MenuButton,
  MenuItem,
  MenuList,
  Portal,
  StackDivider,
  VStack,
  Text,
  Box,
  Switch,
  Input,
} from '@chakra-ui/react'
import React from 'react'
import { useRef } from 'react'
import { useState } from 'react'
import { LinQuery, parseQuery, Queries } from '../../util/parseQuery'
import { InfoTooltip } from './InfoTooltip'
import { SliderWithInfo } from './SliderWithInfo'

export interface QueriesPanelProps {
  queries: Queries
  setQueries: any
}

export const QueriesPanel = (props: QueriesPanelProps) => {
  const { queries, setQueries } = props
  const [t, setT] = useState('')
  const q = useRef<LinQuery>()
  return (
    <VStack
      spacing={2}
      justifyContent="flex-start"
      divider={<StackDivider borderColor="gray.500" />}
      align="stretch"
      paddingLeft={7}
      color="gray.800"
    >
      <Flex alignItems="center" justifyContent="space-between">
        <Input
          value={t}
          onChange={(event) => {
            const text = event.target.value
            setT(text)
            const pq = parseQuery(text, {})
            if (q.current?.length !== pq.length) {
              setQueries({ first: { query: pq, mode: 'white' } })
            }
            q.current = pq
          }}
        />
      </Flex>
    </VStack>
  )
}
