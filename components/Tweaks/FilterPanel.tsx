import { ChevronDownIcon } from '@chakra-ui/icons'
import {
  Text,
  Box,
  Button,
  Flex,
  Menu,
  MenuButton,
  StackDivider,
  VStack,
  Portal,
  MenuList,
  MenuItem,
  Switch,
  Accordion,
  AccordionItem,
  AccordionButton,
  AccordionIcon,
  AccordionPanel,
} from '@chakra-ui/react'
import React from 'react'
import { TagPanel } from './TagPanel'
import { initialFilter, TagColors } from '../config'
import { TagColorPanel } from './TagColorPanel'

export interface FilterPanelProps {
  filter: typeof initialFilter
  setFilter: any
  tagColors: TagColors
  setTagColors: any
  highlightColor: string
  colorList: string[]
  tags: string[]
}

const FilterPanel = (props: FilterPanelProps) => {
  const { filter, setFilter, tagColors, setTagColors, highlightColor, colorList, tags } = props
  return (
    <Box>
      <VStack
        spacing={2}
        justifyContent="flex-start"
        divider={<StackDivider borderColor="gray.500" />}
        align="stretch"
        paddingLeft={7}
        color="gray.800"
      >
        <Flex alignItems="center" justifyContent="space-between">
          <Text>Link children to</Text>
          <Menu isLazy placement="right">
            <MenuButton
              as={Button}
              rightIcon={<ChevronDownIcon />}
              colorScheme=""
              color="black"
              size="sm"
            >
              {(() => {
                switch (filter.parent) {
                  case 'parent':
                    return <Text>File</Text>
                  case 'heading':
                    return <Text>Heading</Text>
                  default:
                    return <Text>Nothing</Text>
                }
              })()}
            </MenuButton>
            <Portal>
              {' '}
              <MenuList bgColor="gray.200" zIndex="popover">
                <MenuItem
                  onClick={() =>
                    setFilter((curr: typeof initialFilter) => ({ ...curr, parent: '' }))
                  }
                >
                  Nothing
                </MenuItem>
                <MenuItem
                  onClick={() =>
                    setFilter((curr: typeof initialFilter) => ({
                      ...curr,
                      parent: 'parent',
                    }))
                  }
                >
                  Parent file node
                </MenuItem>
                <MenuItem
                  onClick={() =>
                    setFilter((curr: typeof initialFilter) => ({
                      ...curr,
                      parent: 'heading',
                    }))
                  }
                >
                  Next highest heading node
                </MenuItem>
              </MenuList>
            </Portal>
          </Menu>
        </Flex>
        <Flex justifyContent="space-between">
          <Text>Orphans</Text>
          <Switch
            onChange={() => {
              setFilter((curr: typeof initialFilter) => {
                return { ...curr, orphans: !curr.orphans }
              })
            }}
            isChecked={filter.orphans}
          ></Switch>
        </Flex>
        <Flex justifyContent="space-between">
          <Text>Dailies</Text>
          <Switch
            onChange={() => {
              setFilter((curr: typeof initialFilter) => {
                return { ...curr, dailies: !curr.dailies }
              })
            }}
            isChecked={filter.dailies}
          ></Switch>
        </Flex>
        <Flex justifyContent="space-between">
          <Text>Citations without note files</Text>
          <Switch
            onChange={() => {
              setFilter({ ...filter, filelessCites: !filter.filelessCites })
            }}
            isChecked={filter.filelessCites}
          ></Switch>
        </Flex>
        <Flex justifyContent="space-between">
          <Text>Non-existant nodes</Text>
          <Switch
            onChange={() => {
              setTagColors({ ...tagColors, bad: 'white' })
              setFilter({ ...filter, bad: !filter.bad })
            }}
            isChecked={filter.bad}
          ></Switch>
        </Flex>
      </VStack>
      <Accordion padding={0} allowToggle allowMultiple paddingLeft={3}>
        <AccordionItem>
          <AccordionButton>
            Tag filters
            <AccordionIcon />
          </AccordionButton>
          <AccordionPanel pr={0} mr={0}>
            <TagPanel
              highlightColor={highlightColor}
              filter={filter}
              setFilter={setFilter}
              tags={tags}
              mode="blacklist"
            />
            <TagPanel
              highlightColor={highlightColor}
              filter={filter}
              setFilter={setFilter}
              tags={tags}
              mode="whitelist"
            />
          </AccordionPanel>
        </AccordionItem>
        <AccordionItem>
          <AccordionButton>
            Tag colors
            <AccordionIcon />
          </AccordionButton>
          <AccordionPanel pr={0} mr={0}>
            <TagColorPanel
              tags={tags}
              colorList={colorList}
              tagColors={tagColors}
              setTagColors={setTagColors}
              highlightColor={highlightColor}
            />
          </AccordionPanel>
        </AccordionItem>
      </Accordion>
    </Box>
  )
}

export default FilterPanel
