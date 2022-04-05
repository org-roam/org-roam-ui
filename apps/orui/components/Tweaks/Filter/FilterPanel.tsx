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
import React, { useContext } from 'react'
import { OptionPanel } from '../OptionPanel'
import { initialFilter, initialLocal, TagColors } from '../../config'
import { TagColorPanel } from './TagColorPanel'
import { SliderWithInfo } from '../SliderWithInfo'
import { VariablesContext } from '../../../util/variablesContext'

export interface FilterPanelProps {
  filter: typeof initialFilter
  setFilter: any
  tagColors: TagColors
  setTagColors: any
  highlightColor: string
  colorList: string[]
  tags: string[]
  local: typeof initialLocal
  setLocal: any
}

const FilterPanel = (props: FilterPanelProps) => {
  const {
    filter,
    setFilter,
    local,
    setLocal,
    tagColors,
    setTagColors,
    highlightColor,
    colorList,
    tags,
  } = props
  const { roamDir, subDirs } = useContext(VariablesContext)
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
          <Text>Org-noter pages</Text>
          <Switch
            onChange={() => {
              setFilter((curr: typeof initialFilter) => {
                return { ...curr, noter: !curr.noter }
              })
            }}
            isChecked={filter.noter}
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
          <Text>Non-existent nodes</Text>
          <Switch
            onChange={() => {
              setTagColors({ ...tagColors, bad: 'white' })
              setFilter({ ...filter, bad: !filter.bad })
            }}
            isChecked={filter.bad}
          ></Switch>
        </Flex>
        <SliderWithInfo
          label="Number of neighbors in local graph"
          value={local.neighbors}
          onChange={(v) => setLocal({ ...local, neighbors: v })}
          min={1}
          max={5}
          step={1}
        />
      </VStack>
      <Accordion padding={0} allowToggle allowMultiple paddingLeft={3}>
        <AccordionItem>
          <AccordionButton>
            Directory filters
            <AccordionIcon />
          </AccordionButton>
          <AccordionPanel pr={0} mr={0}>
            <OptionPanel
              filter={filter}
              setFilter={setFilter}
              options={subDirs}
              displayName="directory blocklist"
              listName="dirsBlocklist"
              labelFilter={roamDir}
            />
            <OptionPanel
              filter={filter}
              setFilter={setFilter}
              options={subDirs}
              displayName="directory allowlist"
              listName="dirsAllowlist"
              labelFilter={roamDir}
            />
          </AccordionPanel>
        </AccordionItem>
        <AccordionItem>
          <AccordionButton>
            Tag filters
            <AccordionIcon />
          </AccordionButton>
          <AccordionPanel pr={0} mr={0}>
            <OptionPanel
              filter={filter}
              setFilter={setFilter}
              options={tags}
              displayName="tag blocklist"
              listName="tagsBlacklist"
            />
            <OptionPanel
              filter={filter}
              setFilter={setFilter}
              options={tags}
              displayName="tag allowlist"
              listName="tagsWhitelist"
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
