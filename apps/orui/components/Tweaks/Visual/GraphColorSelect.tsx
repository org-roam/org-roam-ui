import React, { useContext } from 'react'
import {
  Box,
  Button,
  Flex,
  Menu,
  MenuButton,
  MenuItem,
  MenuList,
  Portal,
  Text,
} from '@chakra-ui/react'
import { ChevronDownIcon } from '@chakra-ui/icons'
import { initialColoring } from '../../config'

export interface GraphColorSelectProps {
  coloring: typeof initialColoring
  setColoring: any
}

export const GraphColorSelect = (props: GraphColorSelectProps) => {
  type Theme = { [key: string]: string }
  const { coloring, setColoring } = props
  return (
    <Flex alignItems="center" justifyContent="space-between" pl={7} pr={2}>
      <Text>Graph coloring</Text>
      <Menu isLazy placement="right">
        <MenuButton
          as={Button}
          size="sm"
          colorScheme=""
          color="black"
          rightIcon={<ChevronDownIcon />}
        >
          {coloring.method === 'degree' ? 'Links' : 'Communities'}
        </MenuButton>
        <Portal>
          <MenuList minW={10} zIndex="popover" bgColor="gray.200">
            <MenuItem
              onClick={() =>
                setColoring((curr: typeof initialColoring) => ({ ...curr, method: 'degree' }))
              }
              justifyContent="space-between"
              alignItems="center"
              display="flex"
            >
              Number of links
            </MenuItem>
            <MenuItem
              onClick={() =>
                setColoring((curr: typeof initialColoring) => ({ ...curr, method: 'community' }))
              }
              justifyContent="space-between"
              alignItems="center"
              display="flex"
            >
              Communities
            </MenuItem>
          </MenuList>
        </Portal>
      </Menu>
    </Flex>
  )
}
