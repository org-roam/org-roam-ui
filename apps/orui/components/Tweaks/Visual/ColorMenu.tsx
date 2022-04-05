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
  PopoverTrigger,
  PopoverContent,
  Popover,
} from '@chakra-ui/react'
import React, { useCallback } from 'react'
import { initialVisuals } from '../../config'

export interface ColorMenuProps {
  label: string
  colorList: string[]
  value: string
  visValue: string
  setVisuals?: any
  noEmpty?: boolean
}

export const ColorMenu = (props: ColorMenuProps) => {
  const { label, colorList, value, visValue, setVisuals, noEmpty } = props

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
      <Popover isLazy placement="right">
        <PopoverTrigger>
          <Button colorScheme="" color="black" rightIcon={<ChevronDownIcon />}>
            {<Box bgColor={visValue} borderRadius="sm" height={6} width={6}></Box>}
          </Button>
        </PopoverTrigger>
        <Portal>
          <PopoverContent zIndex="tooltip" maxW={36} position="relative">
            <Flex flexWrap="wrap" bgColor="gray.200">
              {!noEmpty && (
                <Box
                  onClick={() => clickCallback('')}
                  justifyContent="space-between"
                  alignItems="center"
                  display="flex"
                  m={1}
                >
                  <Box
                    height={6}
                    width={6}
                    borderColor="gray.600"
                    borderRadius="xl"
                    borderWidth={1}
                  ></Box>
                </Box>
              )}
              {colorList.map((color: string) => (
                <Box
                  m={1}
                  key={color}
                  onClick={() => clickCallback(color)}
                  justifyContent="space-between"
                  alignItems="center"
                  display="flex"
                >
                  <Box bgColor={color} borderRadius="xl" height={6} width={6}></Box>
                </Box>
              ))}
            </Flex>
          </PopoverContent>
        </Portal>
      </Popover>
    </Flex>
  )
}
