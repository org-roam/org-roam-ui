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

export interface GraphColorSelectProps {
  coloring: string
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
          {coloring === 'degree' ? 'Links' : 'Communities'}
        </MenuButton>
        <Portal>
          <MenuList minW={10} zIndex="popover" bgColor="gray.200">
            <MenuItem
              onClick={() => setColoring('degree')}
              justifyContent="space-between"
              alignItems="center"
              display="flex"
            >
              Number of links
            </MenuItem>
            <MenuItem
              onClick={() => setColoring('community')}
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
