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

import { themes } from '../themes'
import { ChevronDownIcon } from '@chakra-ui/icons'
import { ThemeContext } from '../../util/themecontext'

export const ThemeSelect = () => {
  type Theme = { [key: string]: string }
  const { emacsTheme, setEmacsTheme, highlightColor } = useContext(ThemeContext)
  return (
    <Flex alignItems="center" justifyContent="space-between" pl={7} pr={2}>
      <Text>Theme</Text>
      <Menu isLazy placement="bottom" closeOnSelect={false}>
        <MenuButton as={Button} colorScheme="" color="black" rightIcon={<ChevronDownIcon />}>
          {emacsTheme[0]}
        </MenuButton>
        <MenuList minW={10} zIndex="popover" bgColor="gray.200">
          <MenuItem
            onClick={() => ''}
            justifyContent="space-between"
            alignItems="center"
            display="flex"
          >
            <Box height={6} width={6}></Box>
          </MenuItem>
          {Object.keys(themes).map((theme: string, i: number) => (
            <MenuItem
              key={theme}
              onClick={() => setEmacsTheme([theme, themes[theme]])}
              justifyContent="space-between"
              alignItems="center"
              display="flex"
            >
              <Text>{theme}</Text>
              <Flex height={6} width={20} flexDirection="column" flexWrap="wrap">
                {Object.values(themes[theme as string]).map((color: string) => {
                  return <Box key={color} bgColor={color} flex="1 1 8px"></Box>
                })}
              </Flex>
            </MenuItem>
          ))}
        </MenuList>
      </Menu>
    </Flex>
  )
}
