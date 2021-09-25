import { DeleteIcon } from '@chakra-ui/icons'
import {
  Text,
  Box,
  Flex,
  IconButton,
  Menu,
  MenuButton,
  MenuItem,
  MenuList,
  Portal,
  StackDivider,
  VStack,
  Button,
} from '@chakra-ui/react'
import { CUIAutoComplete } from 'chakra-ui-autocomplete'
import React, { useState } from 'react'
import { TagColors } from '../config'

export interface TagColorPanelProps {
  tags: string[]
  highlightColor: string
  colorList: string[]
  tagColors: TagColors
  setTagColors: any
}
export const TagColorPanel = (props: TagColorPanelProps) => {
  const { colorList, tagColors, setTagColors, highlightColor, tags } = props
  const tagArray = tags.map((tag) => {
    return { value: tag, label: tag }
  })

  const [selectedItems, setSelectedItems] = useState<typeof tagArray>(
    Object.keys(tagColors).map((tag) => {
      return { value: tag, label: tag }
    }),
  )

  return (
    <Box>
      <CUIAutoComplete
        items={tagArray}
        label="Add tag to filter"
        placeholder=" "
        disableCreateItem={true}
        selectedItems={selectedItems}
        onSelectedItemsChange={(changes) => {
          if (changes.selectedItems) {
            setSelectedItems(Array.from(new Set(changes.selectedItems)))
            setTagColors(
              Object.fromEntries(
                Array.from(new Set(changes.selectedItems)).map((item) => {
                  return [item.label, tagColors[item.label] ?? 'gray.600']
                }),
              ),
            )
          }
        }}
        listItemStyleProps={{ overflow: 'hidden' }}
        highlightItemBg="gray.400"
        toggleButtonStyleProps={{ variant: 'outline' }}
        inputStyleProps={{
          focusBorderColor: highlightColor,
          color: 'gray.800',
          borderColor: 'gray.600',
        }}
        tagStyleProps={{
          display: 'none',
          rounded: 'full',
          bg: highlightColor,
          height: 8,
          paddingLeft: 4,
          fontWeight: 'bold',
        }}
        hideToggleButton
        itemRenderer={(selected) => selected.label}
      />
      <VStack
        spacing={2}
        justifyContent="flex-start"
        divider={<StackDivider borderColor="gray.500" />}
        align="stretch"
        color="gray.800"
      >
        {Object.keys(tagColors).map((tag) => {
          return (
            <Flex key={tag} alignItems="center" justifyContent="space-between" width="100%" pl={2}>
              <Box width="100%">
                <Text fontWeight="bold">{tag}</Text>
              </Box>
              <Menu isLazy placement="right">
                <MenuButton as={Button} colorScheme="" color="black">
                  {<Box bgColor={tagColors[tag]} borderRadius="sm" height={6} width={6}></Box>}
                </MenuButton>
                <Portal>
                  {' '}
                  <MenuList minW={10} zIndex="popover" bgColor="gray.200">
                    {colorList.map((color: string) => (
                      <MenuItem
                        key={color}
                        onClick={() =>
                          setTagColors({
                            ...tagColors,
                            [tag]: color,
                          })
                        }
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
              <IconButton
                aria-label="Delete tag color"
                variant="ghost"
                icon={<DeleteIcon />}
                onClick={() => {
                  setTagColors(
                    Object.fromEntries(
                      Array.from(new Set(selectedItems)).map((item) => {
                        return [item.label, tagColors[item.label] ?? 'gray.600']
                      }),
                    ),
                  )
                  setSelectedItems(selectedItems.filter((item) => item.value !== tag))
                }}
              />
            </Flex>
          )
        })}
      </VStack>
    </Box>
  )
}
