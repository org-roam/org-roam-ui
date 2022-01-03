import { MinusIcon, AddIcon, ViewOffIcon, ViewIcon } from '@chakra-ui/icons'
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
  useDisclosure,
} from '@chakra-ui/react'
import React from 'react'
import { colorList, initialFilter, TagColors } from './config'
import { Collapse } from './Sidebar/Collapse'
import { ColorMenu } from './Tweaks/Visual/ColorMenu'

export interface TagMenuProps {
  setTagColors: any
  tagColors: TagColors
  setFilter: any
  filter: typeof initialFilter
  target: string | null
}

export const TagMenu = (props: TagMenuProps) => {
  const { setTagColors, setFilter, filter, tagColors, target } = props
  const bl: string[] = filter.tagsBlacklist
  const wl: string[] = filter.tagsWhitelist
  const blacklist = bl.indexOf(target as string) > -1
  const whitelist = wl.indexOf(target as string) > -1
  const colors = useDisclosure()
  return (
    <>
      <MenuItem
        icon={
          <Box
            bgColor={tagColors[target as string]}
            borderRadius="sm"
            height={3}
            width={3}
            borderColor={tagColors[target as string] || 'gray.600'}
            borderWidth={1}
          ></Box>
        }
        closeOnSelect={false}
        onClick={colors.onToggle}
      >
        <Text>Change color</Text>
      </MenuItem>
      <Collapse in={colors.isOpen}>
        <Flex ml={2} mt={2} flexWrap="wrap">
          <Box key="empty">
            <Box
              tabIndex={0}
              cursor="pointer"
              onClick={() =>
                setTagColors((curr: { [tag: string]: string }) =>
                  Object.fromEntries(
                    Object.keys(curr)
                      .filter((t) => t !== target)
                      .map((t) => [t, curr[t]]),
                  ),
                )
              }
              bgColor={''}
              m={1}
              borderRadius="sm"
              borderColor="gray.600"
              borderWidth={1}
              height={3}
              width={3}
            ></Box>
          </Box>
          {colorList.map((color: string) => (
            <Box key={color}>
              <Box
                tabIndex={0}
                cursor="pointer"
                onClick={() =>
                  setTagColors({
                    ...tagColors,
                    [target as string]: color,
                  })
                }
                bgColor={color}
                m={1}
                borderRadius="sm"
                height={3}
                width={3}
              ></Box>
            </Box>
          ))}
        </Flex>
      </Collapse>
      {!whitelist && (
        <MenuItem
          onClick={() => {
            if (!blacklist) {
              setFilter((filter: typeof initialFilter) => ({
                ...filter,
                tagsBlacklist: [...filter.tagsBlacklist, target],
              }))
              return
            }
            setFilter((filter: typeof initialFilter) => ({
              ...filter,
              tagsBlacklist: filter.tagsBlacklist.filter((t) => t !== target),
            }))
          }}
          icon={blacklist ? <MinusIcon /> : <ViewOffIcon />}
        >
          {blacklist ? 'Remove from blocklist' : 'Add to blocklist'}
        </MenuItem>
      )}
      {!blacklist && (
        <MenuItem
          onClick={() => {
            if (!whitelist) {
              setFilter((filter: typeof initialFilter) => ({
                ...filter,
                tagsWhitelist: [...filter.tagsWhitelist, target],
              }))
              return
            }
            setFilter((filter: typeof initialFilter) => ({
              ...filter,
              tagsWhitelist: filter.tagsWhitelist.filter((t) => t !== target),
            }))
          }}
          icon={whitelist ? <MinusIcon /> : <ViewIcon />}
        >
          {whitelist ? 'Remove from allowlist' : 'Add to allowlist'}
        </MenuItem>
      )}
    </>
  )
}
