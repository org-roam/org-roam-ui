import { ChevronDownIcon } from '@chakra-ui/icons'
import { Button, Menu, MenuButton, MenuItem, MenuList, Portal } from '@chakra-ui/react'
import React from 'react'

export interface DropDownMenuProps {
  textArray: string[]
  onClickArray: (() => void)[]
  displayValue: string
}

export const DropDownMenu = (props: DropDownMenuProps) => {
  const { textArray, onClickArray, displayValue } = props
  return (
    <Menu isLazy placement="right">
      <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
        {displayValue}
      </MenuButton>
      <Portal>
        {' '}
        <MenuList zIndex="popover">
          {textArray.map((option, i) => {
            ;<MenuItem onClick={onClickArray[i]}> {option} </MenuItem>
          })}
        </MenuList>
      </Portal>
    </Menu>
  )
}
