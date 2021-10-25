import { CheckIcon, ChevronDownIcon, ViewIcon, ViewOffIcon } from '@chakra-ui/icons'
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
  IconButton,
  CloseButton,
} from '@chakra-ui/react'
import React from 'react'
import { useRef } from 'react'
import { useState } from 'react'
import { LinQuery, parseQuery, LinQueries, Query } from '../../util/parseQuery'
import { generateSlug } from 'random-word-slugs'
import { omit } from 'lodash'
import { colorList } from '../config'
import { ColorMenu } from './ColorMenu'

export interface QueriesPanelProps {
  queries: LinQueries
  setQueries: any
}

export const QueriesPanel = (props: QueriesPanelProps) => {
  const { queries, setQueries } = props
  const [t, setT] = useState('')
  const q = useRef<LinQuery>()
  const randoWordOpt = {
    format: 'kebab',
    partsOfSpeech: ['noun'],
    categories: {
      adjective: ['color'],
      noun: ['education', 'technology', 'media', 'science'],
    },
  }
  const modeList = ['black', 'white', 'color']
  const iconsList = [
    <ViewOffIcon key="black" />,
    <ViewIcon key="white" />,
    <CheckIcon key="color" />,
  ]
  return (
    <VStack
      spacing={2}
      justifyContent="flex-start"
      divider={<StackDivider borderColor="gray.500" />}
      align="stretch"
      paddingLeft={7}
      color="gray.800"
    >
      <Flex flexDir="column" alignItems="center" justifyContent="space-between">
        <Input
          bg="white"
          value={t}
          onKeyDown={(e) => {
            if (e.key === 'Enter') {
              const pq = parseQuery(t, {})
              const color = colorList[Math.floor(Math.random() * (colorList.length - 1))]
              const cname = color.replaceAll(/(\w+)\.?.*/g, '$1')
              const word = generateSlug(1, randoWordOpt)
              const name = `${cname}-${word}`
              setQueries((old: LinQueries) => ({
                ...omit(old, 'temp'),
                [name]: { query: pq, mode: 'white', color: color },
              }))
              setT('')
            }
          }}
          onChange={(event) => {
            const text = event.target.value
            setT(text)
            const pq = parseQuery(text, queries)
            q.current = pq
            if (text.length === 0) {
              setQueries((old: LinQueries) => ({ ...old, temp: {} }))
              return
            }
            if (q.current?.length !== pq.length) {
              setQueries((old: LinQueries) => ({ ...old, temp: { query: pq, mode: 'white' } }))
            }
          }}
        />
      </Flex>
      {Object.entries(queries).map((entry) => {
        const [key, val] = entry
        if (key === 'temp') return null
        return (
          <Flex alignItems="center" justifyContent="space-between" key={key}>
            <QueryNameInput {...{ setQueries, entry }} />
            <Flex alignItems="center">
              <ColorMenu
                onClick={(color: string) =>
                  setQueries((curr: LinQueries) => ({
                    ...curr,
                    [key]: { ...val, color: color },
                  }))
                }
                stateVal={queries[key].color}
                colorList={colorList}
              />
              <IconButton
                _outline={{}}
                variant="subtle"
                aria-label="Toggle visibility"
                icon={iconsList[modeList.indexOf(val.mode)]}
                onClick={() => {
                  setQueries((old: LinQueries) => ({
                    ...old,
                    [key]: { ...val, mode: modeList[(modeList.indexOf(val.mode) + 1) % 3] },
                  }))
                }}
              />
              <CloseButton onClick={() => setQueries((old: LinQueries) => omit(old, key))} />
            </Flex>
          </Flex>
        )
      })}
    </VStack>
  )
}

export interface QueryNameInputProps {
  entry: (string | { mode: string; query: LinQuery; color: string })[]
  setQueries: any
}

export const QueryNameInput = (props: QueryNameInputProps) => {
  const { entry, setQueries } = props
  const [key, val] = entry
  const [text, setText] = useState(key as string)
  return (
    <Input
      variant="unstyled"
      value={text}
      onChange={(e) => setText(e.target.value)}
      onKeyDown={(k) => {
        if (k.key === 'Enter') {
          setQueries((old: LinQueries) => {
            old[text] = old[key as string]
            return old
          })
        }
      }}
    />
  )
}
