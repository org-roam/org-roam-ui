import {
  CloseIcon,
  RepeatClockIcon,
  ChevronDownIcon,
  SettingsIcon,
  RepeatIcon,
  ArrowRightIcon,
} from '@chakra-ui/icons'
import {
  Accordion,
  AccordionButton,
  AccordionIcon,
  AccordionItem,
  AccordionPanel,
  Box,
  Button,
  Flex,
  IconButton,
  Menu,
  MenuButton,
  MenuItem,
  MenuList,
  MenuOptionGroup,
  MenuItemOption,
  Select,
  StackDivider,
  Switch,
  Text,
  Tooltip,
  VStack,
  Heading,
  Collapse,
  Portal,
} from '@chakra-ui/react'

import React, { useContext, useCallback } from 'react'
import Scrollbars from 'react-custom-scrollbars-2'
import {
  initialPhysics,
  initialFilter,
  initialVisuals,
  initialMouse,
  initialBehavior,
  TagColors,
  colorList,
} from './config'

import FilterPanel from './FilterPanel'
import { ColorMenu } from './ColorMenu'

import { ThemeContext } from '../util/themecontext'
import { usePersistantState } from '../util/persistant-state'
import { SliderWithInfo } from './SliderWithInfo'
import { EnableSection } from './EnableSection'
import { InfoTooltip } from './InfoTooltip'
import { PhysicsPanel } from './PhysicsPanel'
import { VisualsPanel } from './VisualsPanel'

export interface TweakProps {
  physics: typeof initialPhysics
  setPhysics: any
  threeDim: boolean
  setThreeDim: (newValue: boolean) => void
  filter: typeof initialFilter
  setFilter: any
  visuals: typeof initialVisuals
  setVisuals: any
  mouse: typeof initialMouse
  setMouse: any
  behavior: typeof initialBehavior
  setBehavior: any
  tags: string[]
  tagColors: TagColors
  setTagColors: any
}

export const Tweaks = (props: TweakProps) => {
  const {
    physics,
    setPhysics,
    threeDim,
    setThreeDim,
    filter,
    setFilter,
    visuals,
    setVisuals,
    mouse,
    setMouse,
    behavior,
    setBehavior,
    tags,
    tagColors,
    setTagColors,
  } = props
  const [showTweaks, setShowTweaks] = usePersistantState('showTweaks', false)
  const { highlightColor, setHighlightColor } = useContext(ThemeContext)

  return !showTweaks ? (
    <Box
      position="absolute"
      zIndex="overlay"
      marginTop={10}
      marginLeft={10}
      display={showTweaks ? 'none' : 'block'}
    >
      <IconButton
        variant="ghost"
        aria-label="Settings"
        icon={<SettingsIcon />}
        onClick={() => setShowTweaks(true)}
      />
    </Box>
  ) : (
    <Box
      bg="alt.100"
      w="xs"
      marginTop={10}
      marginLeft={10}
      borderRadius="xl"
      paddingBottom={5}
      zIndex={300}
      position="relative"
      boxShadow="xl"
      maxH={0.92 * globalThis.innerHeight}
      marginBottom={10}
    >
      <Box
        display="flex"
        justifyContent="space-between"
        alignItems="center"
        paddingRight={2}
        paddingTop={1}
      >
        <Tooltip label={'Switch to ' + threeDim ? '2D' : '3D' + ' view'}>
          <Button onClick={() => setThreeDim(!threeDim)} variant="ghost" zIndex="overlay">
            {threeDim ? '3D' : '2D'}
          </Button>
        </Tooltip>
        <Box display="flex" alignItems="center">
          <Tooltip label="Reset settings to defaults">
            <IconButton
              aria-label="Reset Defaults"
              icon={<RepeatClockIcon />}
              onClick={() => {
                setVisuals(initialVisuals)
                setFilter(initialFilter)
                setMouse(initialMouse)
                setPhysics(initialPhysics)
                setBehavior(initialBehavior)
              }}
              variant="none"
              size="sm"
            />
          </Tooltip>
          <IconButton
            size="sm"
            icon={<CloseIcon />}
            aria-label="Close Tweak Panel"
            variant="ghost"
            onClick={() => setShowTweaks(false)}
          />
        </Box>
      </Box>
      <Scrollbars
        autoHeight
        autoHeightMax={0.85 * globalThis.innerHeight}
        autoHide
        renderThumbVertical={({ style, ...props }) => (
          <Box
            {...props}
            style={{
              ...style,
              borderRadius: 10,
            }}
            bg={highlightColor}
          />
        )}
      >
        <Accordion allowMultiple allowToggle color="black">
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon marginRight={2} />
              <Heading size="sm">Filter</Heading>
            </AccordionButton>
            <AccordionPanel>
              <FilterPanel
                filter={filter}
                setFilter={setFilter}
                tagColors={tagColors}
                setTagColors={setTagColors}
                highlightColor={highlightColor}
                colorList={colorList}
                tags={tags}
              />
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton display="flex" justifyContent="space-between">
              <Box display="flex">
                <AccordionIcon marginRight={2} />
                <Heading size="sm">Physics</Heading>
              </Box>
            </AccordionButton>
            <AccordionPanel>
              <PhysicsPanel physics={physics} setPhysics={setPhysics} />
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon marginRight={2} />
              <Heading size="sm">Visual</Heading>
            </AccordionButton>
            <AccordionPanel>
              <VisualsPanel
                visuals={visuals}
                setVisuals={setVisuals}
                highlightColor={highlightColor}
                setHighlightColor={setHighlightColor}
                threeDim={threeDim}
              />
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon marginRight={2} />
              <Heading size="sm">Behavior</Heading>
            </AccordionButton>
            <AccordionPanel>
              <VStack
                spacing={2}
                justifyContent="flex-start"
                divider={<StackDivider borderColor="gray.500" />}
                align="stretch"
                paddingLeft={7}
                color="gray.800"
              >
                <Flex alignItems="center" justifyContent="space-between">
                  <Flex>
                    <Text>Expand Node</Text>
                    <InfoTooltip infoText="View only the node and its direct neighbors" />
                  </Flex>
                  <Menu isLazy placement="right">
                    <MenuButton
                      as={Button}
                      rightIcon={<ChevronDownIcon />}
                      colorScheme=""
                      color="black"
                    >
                      <Text>
                        {mouse.local
                          ? mouse.local[0]!.toUpperCase() + mouse.local!.slice(1)
                          : 'Never'}
                      </Text>
                    </MenuButton>
                    <Portal>
                      {' '}
                      <MenuList zIndex="popover" bgColor="gray.200">
                        <MenuItem onClick={() => setMouse({ ...mouse, local: '' })}>Never</MenuItem>
                        <MenuItem onClick={() => setMouse({ ...mouse, local: 'click' })}>
                          Click
                        </MenuItem>
                        <MenuItem onClick={() => setMouse({ ...mouse, local: 'double' })}>
                          Double Click
                        </MenuItem>
                        <MenuItem onClick={() => setMouse({ ...mouse, local: 'right' })}>
                          Right Click
                        </MenuItem>
                      </MenuList>
                    </Portal>
                  </Menu>
                </Flex>
                <Flex alignItems="center" justifyContent="space-between">
                  <Text>Open in Emacs</Text>
                  <Menu isLazy placement="right">
                    <MenuButton
                      as={Button}
                      rightIcon={<ChevronDownIcon />}
                      colorScheme=""
                      color="black"
                    >
                      <Text>
                        {mouse.follow
                          ? mouse.follow[0]!.toUpperCase() + mouse.follow!.slice(1)
                          : 'Never'}
                      </Text>
                    </MenuButton>
                    <Portal>
                      {' '}
                      <MenuList bgColor="gray.200" zIndex="popover">
                        <MenuItem onClick={() => setMouse({ ...mouse, follow: '' })}>
                          Never
                        </MenuItem>
                        <MenuItem onClick={() => setMouse({ ...mouse, follow: 'click' })}>
                          Click
                        </MenuItem>
                        <MenuItem onClick={() => setMouse({ ...mouse, follow: 'double' })}>
                          Double Click
                        </MenuItem>
                        <MenuItem onClick={() => setMouse({ ...mouse, follow: 'right' })}>
                          Right Click
                        </MenuItem>
                      </MenuList>
                    </Portal>
                  </Menu>
                </Flex>
                <Flex alignItems="center" justifyContent="space-between">
                  <Text>Follow Emacs by...</Text>
                  <Menu isLazy placement="right">
                    <MenuButton
                      as={Button}
                      rightIcon={<ChevronDownIcon />}
                      colorScheme=""
                      color="black"
                    >
                      <Text>{behavior.follow[0].toUpperCase() + behavior.follow.slice(1)}</Text>
                    </MenuButton>
                    <Portal>
                      {' '}
                      <MenuList bgColor="gray.200" zIndex="popover">
                        <MenuItem onClick={() => setBehavior({ ...behavior, follow: 'color' })}>
                          Just coloring the currently opened node
                        </MenuItem>
                        <MenuItem onClick={() => setBehavior({ ...behavior, follow: 'local' })}>
                          Opening the local graph
                        </MenuItem>
                        <MenuItem onClick={() => setBehavior({ ...behavior, follow: 'zoom' })}>
                          Zooming to the current node
                        </MenuItem>
                      </MenuList>
                    </Portal>
                  </Menu>
                </Flex>
                <Flex alignItems="center" justifyContent="space-between">
                  <Flex>
                    <Text>Local graph</Text>
                    <InfoTooltip infoText="When in local mode and clicking a new node, should I add that node's local graph or open the new one?" />
                  </Flex>
                  <Menu isLazy placement="right">
                    <MenuButton
                      as={Button}
                      rightIcon={<ChevronDownIcon />}
                      colorScheme=""
                      color="black"
                    >
                      <Text>{behavior.localSame === 'add' ? 'Add' : 'Replace'}</Text>
                    </MenuButton>
                    <Portal>
                      {' '}
                      <MenuList bgColor="gray.200" zIndex="popover">
                        <MenuItem
                          onClick={() => setBehavior({ ...behavior, localSame: 'replace' })}
                        >
                          Open that nodes graph
                        </MenuItem>
                        <MenuItem onClick={() => setBehavior({ ...behavior, localSame: 'add' })}>
                          Add node to local graph
                        </MenuItem>
                      </MenuList>
                    </Portal>
                  </Menu>
                </Flex>
                <SliderWithInfo
                  label="Zoom speed"
                  value={behavior.zoomSpeed}
                  min={0}
                  max={4000}
                  step={100}
                  onChange={(value) => setBehavior({ ...behavior, zoomSpeed: value })}
                />
                <SliderWithInfo
                  label="Zoom padding"
                  value={behavior.zoomPadding}
                  min={0}
                  max={400}
                  step={1}
                  onChange={(value) => setBehavior({ ...behavior, zoomPadding: value })}
                  infoText="How much to zoom out to accomodate all nodes when changing the view."
                />
              </VStack>
            </AccordionPanel>
          </AccordionItem>
        </Accordion>
      </Scrollbars>
    </Box>
  )
}

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
