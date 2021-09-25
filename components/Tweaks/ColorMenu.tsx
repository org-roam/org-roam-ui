import { ChevronDownIcon } from '@chakra-ui/icons'
import {
  Text,
  Box,
  Button,
  Flex,
  Menu,
  MenuButton,
  MenuItem,
  MenuList,
  Portal,
} from '@chakra-ui/react'
import React, { useCallback } from 'react'
import { initialVisuals } from '../config'

export interface ColorMenuProps {
  label: string
  colorList: string[]
  value: string
  visValue: string
  setVisuals?: any
}

export const ColorMenu = (props: ColorMenuProps) => {
  const { label, colorList, value, visValue, setVisuals } = props

  const clickCallback = useCallback(
    (color) =>
      setVisuals((curr: typeof initialVisuals) => {
        return {
          ...curr,
          [value]: color,
        }
      }),
    [],
  )
  return (
    <Flex alignItems="center" justifyContent="space-between">
      <Text>{label}</Text>
      <Menu isLazy placement="right">
        <MenuButton as={Button} colorScheme="" color="black" rightIcon={<ChevronDownIcon />}>
          {<Box bgColor={visValue} borderRadius="sm" height={6} width={6}></Box>}
        </MenuButton>
        <Portal>
          {' '}
          <MenuList minW={10} zIndex="popover" bgColor="gray.200">
            <MenuItem
              onClick={() => clickCallback('')}
              justifyContent="space-between"
              alignItems="center"
              display="flex"
            >
              <Box height={6} width={6}></Box>
            </MenuItem>
            {colorList.map((color: string) => (
              <MenuItem
                key={color}
                onClick={() => clickCallback(color)}
                justifyContent="space-between"
                alignItems="center"
                display="flex"
              >
                <Box bgColor={color} borderRadius="sm" height={6} width={6}></Box>
              </MenuItem>
            ))}
          </MenuList>
        </Portal>
      </Menu>
    </Flex>
  )
}
