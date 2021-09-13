import {
  CloseIcon,
  RepeatClockIcon,
  ChevronDownIcon,
  SettingsIcon,
  InfoOutlineIcon,
  RepeatIcon,
  ArrowRightIcon,
  AddIcon,
  DeleteIcon,
  CheckCircleIcon,
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
  Slider,
  SliderFilledTrack,
  SliderThumb,
  SliderTrack,
  StackDivider,
  Switch,
  Text,
  Tooltip,
  VStack,
  Heading,
  Collapse,
  Grid,
  Portal,
  SlideFade,
  Input,
} from '@chakra-ui/react'
import { CUIAutoComplete } from 'chakra-ui-autocomplete'

import React, { useState, useContext, useEffect, useCallback } from 'react'
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

import { ThemeContext } from '../util/themecontext'
import { usePersistantState } from '../util/persistant-state'

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

  const setVisualsCallback = useCallback((val) => setVisuals(val), [])
  const setPhysicsCallback = useCallback((val: number, phys: string, scale: number) => {
    setPhysics((curr: typeof initialPhysics) => {
      return { ...curr, [phys]: val / scale }
    })
  }, [])

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
              <VStack
                spacing={2}
                justifyContent="flex-start"
                divider={<StackDivider borderColor="gray.500" />}
                align="stretch"
                paddingLeft={7}
                color="gray.800"
              >
                <Flex alignItems="center" justifyContent="space-between">
                  <Text>Link children to...</Text>
                  <Menu isLazy placement="right">
                    <MenuButton
                      as={Button}
                      rightIcon={<ChevronDownIcon />}
                      colorScheme=""
                      color="black"
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
                    Tag Colors
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
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton display="flex" justifyContent="space-between">
              <Box display="flex">
                <AccordionIcon marginRight={2} />
                <Heading size="sm">Physics</Heading>
              </Box>
              {/* <Switch
                      id="physicsOn"
                      onChange={() => setPhysics({ ...physics, enabled: !physics.enabled })}
                      isChecked={physics.enabled}
                      /> */}
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
                <EnableSection
                  label="Gravity"
                  value={physics.gravityOn}
                  onChange={() => setPhysics({ ...physics, gravityOn: !physics.gravityOn })}
                >
                  <Flex justifyContent="space-between">
                    <Text>Also in local</Text>
                    <Switch
                      onChange={() => {
                        setPhysics((curr: typeof initialPhysics) => {
                          return { ...curr, gravityLocal: !curr.gravityLocal }
                        })
                      }}
                      isChecked={physics.gravityLocal}
                    ></Switch>
                  </Flex>
                  <SliderWithInfo
                    label="Strength"
                    value={physics.gravity * 10}
                    onChange={(v: number) => setPhysicsCallback(v, 'gravity', 10)}
                  />
                </EnableSection>
                <SliderWithInfo
                  value={-physics.charge / 100}
                  onChange={(v) => setPhysicsCallback(v, 'charge', -1 / 100)}
                  label="Repulsive Force"
                />
                <EnableSection
                  label="Collision"
                  infoText="Perfomance sap, disable if slow"
                  value={physics.collision}
                  onChange={() => setPhysics({ ...physics, collision: !physics.collision })}
                >
                  <SliderWithInfo
                    value={physics.collisionStrength / 5}
                    onChange={(v) => setPhysicsCallback(v, 'collisionStrength', 1 / 5)}
                    label="Collision Radius"
                    infoText="Easy with this one, high values can lead to a real jiggly mess"
                  />
                </EnableSection>
                <SliderWithInfo
                  value={physics.linkStrength * 5}
                  onChange={(v) => setPhysicsCallback(v, 'linkStrength', 5)}
                  label="Link Force"
                />
                <SliderWithInfo
                  label="Link Iterations"
                  value={physics.linkIts}
                  onChange={(v) => setPhysicsCallback(v, 'linkIts', 1)}
                  min={0}
                  max={6}
                  step={1}
                  infoText="How many links down the line the physics of a single node affects (Slow)"
                />
                <SliderWithInfo
                  label="Viscosity"
                  value={physics.velocityDecay * 10}
                  onChange={(v) => setPhysicsCallback(v, 'velocityDecay', 10)}
                />
              </VStack>
              <Box>
                <Accordion paddingLeft={3} allowToggle>
                  <AccordionItem>
                    <AccordionButton>
                      <Text>Advanced</Text>
                      <AccordionIcon marginRight={2} />
                    </AccordionButton>
                    <AccordionPanel>
                      <VStack
                        spacing={2}
                        justifyContent="flex-start"
                        divider={<StackDivider borderColor="gray.500" />}
                        align="stretch"
                        paddingLeft={3}
                        color="gray.800"
                      >
                        <SliderWithInfo
                          label="Stabilization rate"
                          value={physics.alphaDecay * 50}
                          onChange={(v) => setPhysicsCallback(v, 'alphaDecay', 50)}
                        />
                        <EnableSection
                          label="Center nodes"
                          value={physics.centering}
                          onChange={() => setPhysics({ ...physics, centering: !physics.centering })}
                          infoText="Keeps the nodes in the center of the viewport. If disabled you can drag the nodes anywhere you want."
                        >
                          <SliderWithInfo
                            label="Centering Strength"
                            value={physics.centeringStrength}
                            max={2}
                            step={0.01}
                            onChange={(v) => setPhysicsCallback(v, 'centeringStrength', 1)}
                          />
                        </EnableSection>
                      </VStack>
                    </AccordionPanel>
                  </AccordionItem>
                </Accordion>
              </Box>
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon marginRight={2} />
              <Heading size="sm">Visual</Heading>
            </AccordionButton>
            <AccordionPanel>
              <VStack justifyContent="flex-start" align="stretch">
                <Accordion allowToggle defaultIndex={[0]} paddingLeft={3}>
                  <AccordionItem>
                    <AccordionButton>
                      <Flex justifyContent="space-between" w="100%">
                        <Text>Colors</Text>
                        <AccordionIcon marginRight={2} />
                      </Flex>
                    </AccordionButton>
                    <AccordionPanel>
                      <VStack
                        spacing={2}
                        justifyContent="flex-start"
                        divider={<StackDivider borderColor="gray.500" />}
                        align="stretch"
                        color="gray.800"
                      >
                        <Box>
                          <Flex alignItems="center" justifyContent="space-between">
                            <Text>Nodes</Text>
                            <Tooltip label="Shuffle node colors">
                              <IconButton
                                aria-label="Shuffle node colors"
                                size="sm"
                                icon={<RepeatIcon />}
                                variant="ghost"
                                onClick={() => {
                                  const arr = visuals.nodeColorScheme ?? []
                                  setVisuals({
                                    ...visuals,
                                    //shuffle that guy
                                    //definitely thought of this myself
                                    nodeColorScheme: arr
                                      .map((x: any) => [Math.random(), x])
                                      .sort(([a], [b]) => a - b)
                                      .map(([_, x]) => x),
                                  })
                                }}
                              />
                            </Tooltip>
                            <Tooltip label="Cycle node colors">
                              <IconButton
                                aria-label="Shift node colors"
                                icon={<ArrowRightIcon />}
                                size="sm"
                                variant="ghost"
                                onClick={() => {
                                  const arr = visuals.nodeColorScheme ?? []
                                  setVisuals({
                                    ...visuals,
                                    nodeColorScheme: [...arr.slice(1, arr.length), arr[0]],
                                  })
                                }}
                              />
                            </Tooltip>
                            <Menu isLazy placement="right" closeOnSelect={false} matchWidth>
                              <MenuButton
                                width={20}
                                as={Button}
                                colorScheme=""
                                color="black"
                                rightIcon={<ChevronDownIcon />}
                              >
                                <Flex height={6} width={6} flexDirection="column" flexWrap="wrap">
                                  {visuals.nodeColorScheme.map((color) => (
                                    <Box
                                      key={color}
                                      bgColor={color}
                                      flex="1 1 8px"
                                      borderRadius="2xl"
                                    ></Box>
                                  ))}
                                </Flex>
                              </MenuButton>
                              <Portal>
                                {' '}
                                <MenuList minW={10} zIndex="popover" bgColor="gray.200">
                                  <MenuOptionGroup
                                    width={500}
                                    type="checkbox"
                                    defaultValue={visuals.nodeColorScheme}
                                    onChange={(colors) => {
                                      if (!colors.length) {
                                        return
                                      }
                                      setVisuals({ ...visuals, nodeColorScheme: colors })
                                    }}
                                  >
                                    {colorList.map((color) => (
                                      <MenuItemOption
                                        key={color}
                                        isChecked={visuals.nodeColorScheme.some((c) => c === color)}
                                        value={color}
                                        isDisabled={
                                          visuals.nodeColorScheme.length === 1 &&
                                          visuals.nodeColorScheme[0] === color
                                        }
                                      >
                                        <Box
                                          justifyContent="space-between"
                                          alignItems="center"
                                          display="flex"
                                        >
                                          <Box
                                            bgColor={color}
                                            borderRadius="sm"
                                            height={6}
                                            width={6}
                                          ></Box>
                                        </Box>
                                      </MenuItemOption>
                                    ))}
                                  </MenuOptionGroup>
                                </MenuList>
                              </Portal>
                            </Menu>
                          </Flex>
                          <Flex alignItems="center" justifyContent="space-between">
                            <Text>Links</Text>
                            <Menu isLazy placement="right">
                              <MenuButton
                                as={Button}
                                colorScheme=""
                                color="black"
                                rightIcon={<ChevronDownIcon />}
                              >
                                <Box>
                                  {visuals.linkColorScheme ? (
                                    <Box
                                      bgColor={visuals.linkColorScheme}
                                      borderRadius="sm"
                                      height={6}
                                      width={6}
                                    ></Box>
                                  ) : (
                                    <Flex
                                      height={6}
                                      width={6}
                                      flexDirection="column"
                                      flexWrap="wrap"
                                    >
                                      {visuals.nodeColorScheme.map((color) => (
                                        <Box
                                          key={color}
                                          bgColor={color}
                                          flex="1 1 8px"
                                          borderRadius="2xl"
                                        ></Box>
                                      ))}
                                    </Flex>
                                  )}
                                </Box>
                              </MenuButton>
                              <Portal>
                                {' '}
                                <MenuList minW={10} zIndex="popover" bgColor="gray.200">
                                  <MenuItem
                                    onClick={() => setVisuals({ ...visuals, linkColorScheme: '' })}
                                    justifyContent="space-between"
                                    alignItems="center"
                                    display="flex"
                                  >
                                    <Flex
                                      height={6}
                                      width={6}
                                      flexDirection="column"
                                      flexWrap="wrap"
                                    >
                                      {visuals.nodeColorScheme.map((color) => (
                                        <Box
                                          key={color}
                                          bgColor={color}
                                          flex="1 1 8px"
                                          borderRadius="2xl"
                                        ></Box>
                                      ))}
                                    </Flex>
                                  </MenuItem>
                                  {colorList.map((color) => (
                                    <MenuItem
                                      key={color}
                                      onClick={() =>
                                        setVisuals({
                                          ...visuals,
                                          linkColorScheme: color,
                                        })
                                      }
                                      justifyContent="space-between"
                                      alignItems="center"
                                      display="flex"
                                    >
                                      <Box
                                        bgColor={color}
                                        borderRadius="sm"
                                        height={6}
                                        width={6}
                                      ></Box>
                                    </MenuItem>
                                  ))}
                                </MenuList>
                              </Portal>
                            </Menu>
                          </Flex>
                          <Flex alignItems="center" justifyContent="space-between">
                            <Text>Accent</Text>
                            <Menu isLazy placement="right">
                              <MenuButton
                                as={Button}
                                colorScheme=""
                                color="black"
                                rightIcon={<ChevronDownIcon />}
                              >
                                {
                                  <Box
                                    bgColor={highlightColor}
                                    borderRadius="sm"
                                    height={6}
                                    width={6}
                                  ></Box>
                                }
                              </MenuButton>
                              <Portal>
                                {' '}
                                <MenuList minW={10} zIndex="popover" bgColor="gray.200">
                                  {colorList.map((color) => (
                                    <MenuItem
                                      key={color}
                                      onClick={() => setHighlightColor(color)}
                                      justifyContent="space-between"
                                      alignItems="center"
                                      display="flex"
                                    >
                                      <Box
                                        bgColor={color}
                                        borderRadius="sm"
                                        height={6}
                                        width={6}
                                      ></Box>
                                    </MenuItem>
                                  ))}
                                </MenuList>
                              </Portal>
                            </Menu>
                          </Flex>
                          <ColorMenu
                            colorList={colorList}
                            label="Link highlight"
                            setVisuals={setVisualsCallback}
                            value="linkHighlight"
                            visValue={visuals.linkHighlight}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Node highlight"
                            setVisuals={setVisualsCallback}
                            value="nodeHighlight"
                            visValue={visuals.nodeHighlight}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Background"
                            setVisuals={setVisualsCallback}
                            value="backgroundColor"
                            visValue={visuals.backgroundColor}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Emacs node"
                            setVisuals={setVisualsCallback}
                            value="emacsNodeColor"
                            visValue={visuals.emacsNodeColor}
                          />
                        </Box>
                      </VStack>
                    </AccordionPanel>
                  </AccordionItem>
                  <AccordionItem>
                    <AccordionButton>
                      <Flex justifyContent="space-between" w="100%">
                        <Text>Nodes & Links</Text>
                        <AccordionIcon marginRight={2} />
                      </Flex>
                    </AccordionButton>
                    <AccordionPanel>
                      <VStack
                        spacing={2}
                        justifyContent="flex-start"
                        divider={<StackDivider borderColor="gray.500" />}
                        align="stretch"
                        color="gray.800"
                      >
                        <Box>
                          <SliderWithInfo
                            label="Node size"
                            value={visuals.nodeRel}
                            onChange={(value) => setVisuals({ ...visuals, nodeRel: value })}
                          />
                          <SliderWithInfo
                            label="Node connections size scale"
                            value={visuals.nodeSizeLinks}
                            min={0}
                            max={2}
                            onChange={(value) => setVisuals({ ...visuals, nodeSizeLinks: value })}
                          />
                          <SliderWithInfo
                            label="Node zoom invariance"
                            value={visuals.nodeZoomSize}
                            min={0}
                            max={2}
                            infoText="How much the graph will try to keep the nodesize consistent across zoom scales. 0 is no consistency, node will always be their true size, 1 is linear, 2 is quadratic."
                            onChange={(value) =>
                              setVisuals((prev: typeof initialVisuals) => ({
                                ...prev,
                                nodeZoomSize: value,
                              }))
                            }
                          />
                          {threeDim && (
                            <>
                              <SliderWithInfo
                                label="Node opacity"
                                value={visuals.nodeOpacity}
                                min={0}
                                max={1}
                                onChange={(value) => setVisuals({ ...visuals, nodeOpacity: value })}
                              />
                              <SliderWithInfo
                                label="Node resolution"
                                value={visuals.nodeResolution}
                                min={5}
                                max={32}
                                step={1}
                                onChange={(value) =>
                                  setVisuals({ ...visuals, nodeResolution: value })
                                }
                              />
                            </>
                          )}
                          <SliderWithInfo
                            label="Link width"
                            value={visuals.linkWidth}
                            onChange={(value) => setVisuals({ ...visuals, linkWidth: value })}
                          />
                          {threeDim && (
                            <SliderWithInfo
                              label="Link opacity"
                              min={0}
                              max={1}
                              value={visuals.linkOpacity}
                              onChange={(value) => setVisuals({ ...visuals, linkOpacity: value })}
                            />
                          )}
                          <EnableSection
                            label="Link arrows"
                            value={visuals.arrows}
                            onChange={() => setVisuals({ ...visuals, arrows: !visuals.arrows })}
                          >
                            <SliderWithInfo
                              label="Arrow size"
                              value={visuals.arrowsLength / 10}
                              onChange={(value) =>
                                setVisuals({ ...visuals, arrowsLength: 10 * value })
                              }
                            />
                            <SliderWithInfo
                              label="Arrow Position"
                              value={visuals.arrowsPos}
                              min={0}
                              max={1}
                              step={0.01}
                              onChange={(value) => setVisuals({ ...visuals, arrowsPos: value })}
                            />
                            <ColorMenu
                              colorList={colorList}
                              label="Arrow Color"
                              key="arrow"
                              setVisuals={setVisualsCallback}
                              value="arrowsColor"
                              visValue={visuals.arrowsColor}
                            />
                          </EnableSection>
                          <EnableSection
                            label="Directional Particles"
                            value={visuals.particles}
                            onChange={() =>
                              setVisuals({ ...visuals, particles: !visuals.particles })
                            }
                          >
                            <SliderWithInfo
                              label="Particle Number"
                              value={visuals.particlesNumber}
                              max={5}
                              step={1}
                              onChange={(value) =>
                                setVisuals({ ...visuals, particlesNumber: value })
                              }
                            />
                            <SliderWithInfo
                              label="Particle Size"
                              value={visuals.particlesWidth}
                              onChange={(value) =>
                                setVisuals({ ...visuals, particlesWidth: value })
                              }
                            />
                          </EnableSection>
                        </Box>
                      </VStack>
                    </AccordionPanel>
                  </AccordionItem>
                  {/* <VStack
                      spacing={2}
                      justifyContent="flex-start"
                      divider={<StackDivider borderColor="gray.500" />}
                      align="stretch"
                      paddingLeft={7}
                      color="gray.800"
                      > */}
                  <AccordionItem>
                    <AccordionButton>
                      <Flex justifyContent="space-between" w="100%">
                        <Text>Labels</Text>
                        <AccordionIcon marginRight={2} />
                      </Flex>
                    </AccordionButton>
                    <AccordionPanel>
                      <VStack
                        spacing={2}
                        justifyContent="flex-start"
                        divider={<StackDivider borderColor="gray.500" />}
                        align="stretch"
                        color="gray.800"
                      >
                        <Box>
                          <Flex alignItems="center" justifyContent="space-between">
                            <Text>Show labels</Text>
                            <Menu isLazy placement="right">
                              <MenuButton
                                as={Button}
                                colorScheme=""
                                color="black"
                                rightIcon={<ChevronDownIcon />}
                              >
                                {!visuals.labels
                                  ? 'Never'
                                  : visuals.labels < 2
                                  ? 'On Highlight'
                                  : 'Always'}
                              </MenuButton>
                              <Portal>
                                {' '}
                                <MenuList zIndex="popover" bgColor="gray.200">
                                  <MenuItem onClick={() => setVisuals({ ...visuals, labels: 0 })}>
                                    Never
                                  </MenuItem>
                                  <MenuItem onClick={() => setVisuals({ ...visuals, labels: 1 })}>
                                    On Highlight
                                  </MenuItem>
                                  <MenuItem onClick={() => setVisuals({ ...visuals, labels: 2 })}>
                                    Always
                                  </MenuItem>
                                  <MenuItem onClick={() => setVisuals({ ...visuals, labels: 3 })}>
                                    Always (even in 3D)
                                  </MenuItem>
                                </MenuList>
                              </Portal>
                            </Menu>
                          </Flex>
                          <VStack
                            spacing={1}
                            justifyContent="flex-start"
                            divider={<StackDivider borderColor="gray.400" />}
                            align="stretch"
                            paddingLeft={2}
                            color="gray.800"
                          >
                            <SliderWithInfo
                              label="Label font size"
                              value={visuals.labelFontSize}
                              min={5}
                              max={20}
                              step={0.5}
                              onChange={(value) => setVisuals({ ...visuals, labelFontSize: value })}
                            />
                            <SliderWithInfo
                              label="Max. label characters"
                              value={visuals.labelLength}
                              min={10}
                              max={100}
                              step={1}
                              onChange={(value) => setVisuals({ ...visuals, labelLength: value })}
                            />
                            <SliderWithInfo
                              label="Max. label line length"
                              value={visuals.labelWordWrap}
                              min={10}
                              max={100}
                              step={1}
                              onChange={(value) => setVisuals({ ...visuals, labelWordWrap: value })}
                            />
                            <SliderWithInfo
                              label="Space between label lines"
                              value={visuals.labelLineSpace}
                              min={0.2}
                              max={3}
                              step={0.1}
                              onChange={(value) =>
                                setVisuals({ ...visuals, labelLineSpace: value })
                              }
                            />
                            <ColorMenu
                              colorList={colorList}
                              label="Text"
                              setVisuals={setVisualsCallback}
                              value="labelTextColor"
                              visValue={visuals.labelTextColor}
                            />
                            <ColorMenu
                              colorList={colorList}
                              label="Background"
                              setVisuals={setVisualsCallback}
                              value="labelBackgroundColor"
                              visValue={visuals.labelBackgroundColor}
                            />
                            <Collapse in={!!visuals.labelBackgroundColor} animateOpacity>
                              <Box paddingTop={2}>
                                <SliderWithInfo
                                  label="Background opacity"
                                  value={visuals.labelBackgroundOpacity}
                                  onChange={(value) => {
                                    console.log(visuals.labelBackgroundOpacity)
                                    setVisuals({ ...visuals, labelBackgroundOpacity: value })
                                  }}
                                  min={0}
                                  max={1}
                                  step={0.01}
                                />
                              </Box>
                            </Collapse>
                            <Collapse in={visuals.labels > 1} animateOpacity>
                              <Box paddingTop={2}>
                                <SliderWithInfo
                                  label="Label Appearance Scale"
                                  value={visuals.labelScale * 5}
                                  onChange={(value) =>
                                    setVisuals({ ...visuals, labelScale: value / 5 })
                                  }
                                />
                              </Box>
                            </Collapse>
                          </VStack>
                        </Box>
                      </VStack>
                    </AccordionPanel>
                  </AccordionItem>
                  <AccordionItem>
                    <AccordionButton>
                      <Flex justifyContent="space-between" w="100%">
                        <Text>Highlighting</Text>
                        <AccordionIcon marginRight={2} />
                      </Flex>
                    </AccordionButton>
                    <AccordionPanel>
                      <VStack
                        spacing={2}
                        justifyContent="flex-start"
                        divider={<StackDivider borderColor="gray.500" />}
                        align="stretch"
                        color="gray.800"
                      >
                        <Box>
                          <EnableSection
                            label="Highlight"
                            onChange={() =>
                              setVisuals({ ...visuals, highlight: !visuals.highlight })
                            }
                            value={visuals.highlight}
                          >
                            <VStack
                              spacing={1}
                              justifyContent="flex-start"
                              divider={<StackDivider borderColor="gray.400" />}
                              align="stretch"
                              paddingLeft={0}
                            >
                              <SliderWithInfo
                                label="Highlight Link Thickness"
                                value={visuals.highlightLinkSize}
                                onChange={(value) =>
                                  setVisuals({ ...visuals, highlightLinkSize: value })
                                }
                              />
                              <SliderWithInfo
                                label="Highlight Node Size"
                                value={visuals.highlightNodeSize}
                                onChange={(value) =>
                                  setVisuals({ ...visuals, highlightNodeSize: value })
                                }
                              />
                              <SliderWithInfo
                                min={0}
                                max={1}
                                label="Highlight Fade"
                                value={visuals.highlightFade}
                                onChange={(value) =>
                                  setVisuals({ ...visuals, highlightFade: value })
                                }
                              />
                              {/*<Flex justifyContent="space-between">
                          <Text> Highlight node color </Text>
                        </Flex>
                        <Flex justifyContent="space-between">
                          <Text> Highlight link color </Text>
                            </Flex>*/}
                              <EnableSection
                                label="Highlight Animation"
                                onChange={() => {
                                  setVisuals({ ...visuals, highlightAnim: !visuals.highlightAnim })
                                }}
                                value={visuals.highlightAnim}
                              >
                                <SliderWithInfo
                                  label="Animation speed"
                                  onChange={(v) => setVisuals({ ...visuals, animationSpeed: v })}
                                  value={visuals.animationSpeed}
                                  infoText="Slower speed has a chance of being buggy"
                                  min={50}
                                  max={1000}
                                  step={10}
                                />
                                <Select
                                  placeholder={visuals.algorithmName}
                                  onChange={(v) => {
                                    setVisuals({ ...visuals, algorithmName: v.target.value })
                                  }}
                                >
                                  {visuals.algorithmOptions.map((opt: string) => (
                                    <option key={opt} value={opt}>
                                      {opt}
                                    </option>
                                  ))}
                                </Select>
                              </EnableSection>
                            </VStack>
                          </EnableSection>
                        </Box>
                      </VStack>
                    </AccordionPanel>
                  </AccordionItem>
                  <AccordionItem>
                    <AccordionButton>
                      <Flex justifyContent="space-between" w="100%">
                        <Text>Citations</Text>
                        <AccordionIcon marginRight={2} />
                      </Flex>
                    </AccordionButton>
                    <AccordionPanel>
                      <VStack
                        spacing={2}
                        justifyContent="flex-start"
                        divider={<StackDivider borderColor="gray.500" />}
                        align="stretch"
                        color="gray.800"
                      >
                        <Box>
                          <EnableSection
                            label="Dash cite links"
                            infoText="Add dashes to citation links made with org-roam-bibtex"
                            value={visuals.citeDashes}
                            onChange={() =>
                              setVisuals({ ...visuals, citeDashes: !visuals.citeDashes })
                            }
                          >
                            <SliderWithInfo
                              label="Dash length"
                              value={visuals.citeDashLength / 10}
                              onChange={(value) =>
                                setVisuals({ ...visuals, citeDashLength: value * 10 })
                              }
                            />
                            <SliderWithInfo
                              label="Gap length"
                              value={visuals.citeGapLength / 5}
                              onChange={(value) =>
                                setVisuals({ ...visuals, citeGapLength: value * 5 })
                              }
                            />
                          </EnableSection>
                          <ColorMenu
                            colorList={colorList}
                            label="Citation node color"
                            setVisuals={setVisualsCallback}
                            value={'citeNodeColor'}
                            visValue={visuals.citeNodeColor}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Citation link color"
                            setVisuals={setVisualsCallback}
                            value={'citeLinkColor'}
                            visValue={visuals.citeLinkColor}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Reference link highlight"
                            setVisuals={setVisualsCallback}
                            value={'citeLinkHighlightColor'}
                            visValue={visuals.citeLinkHighlightColor}
                          />
                          <EnableSection
                            label="Dash ref links"
                            infoText="Add dashes to citation links, whose target has a note, made with org-roam-bibtex"
                            value={visuals.refDashes}
                            onChange={() =>
                              setVisuals({ ...visuals, refDashes: !visuals.refDashes })
                            }
                          >
                            <SliderWithInfo
                              label="Dash length"
                              value={visuals.refDashLength / 10}
                              onChange={(value) =>
                                setVisuals({ ...visuals, refDashLength: value * 10 })
                              }
                            />
                            <SliderWithInfo
                              label="Gap length"
                              value={visuals.refGapLength / 5}
                              onChange={(value) =>
                                setVisuals({ ...visuals, refGapLength: value * 5 })
                              }
                            />
                          </EnableSection>
                          <ColorMenu
                            colorList={colorList}
                            label="Reference node color"
                            setVisuals={setVisualsCallback}
                            value={'refNodeColor'}
                            visValue={visuals.refNodeColor}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Reference link color"
                            setVisuals={setVisualsCallback}
                            value={'refLinkColor'}
                            visValue={visuals.refLinkColor}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Reference link highlight"
                            setVisuals={setVisualsCallback}
                            value={'refLinkHighlightColor'}
                            visValue={visuals.refLinkHighlightColor}
                          />
                        </Box>
                      </VStack>
                    </AccordionPanel>
                  </AccordionItem>
                </Accordion>
              </VStack>
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

export interface InfoTooltipProps {
  infoText?: string | boolean
}
export const InfoTooltip = (props: InfoTooltipProps) => {
  const { infoText } = props
  return (
    <Box paddingLeft="1">
      <Tooltip label={infoText} placement="top" color="gray.100" bg="gray.800" hasArrow>
        <InfoOutlineIcon />
      </Tooltip>
    </Box>
  )
}
export interface SliderWithInfoProps {
  min?: number
  max?: number
  step?: number
  value: number
  onChange: (arg0: number) => void
  label: string
  infoText?: string
}
export const SliderWithInfo = ({
  min = 0,
  max = 10,
  step = 0.1,
  value = 1,
  ...rest
}: SliderWithInfoProps) => {
  const { onChange, label, infoText } = rest
  const { highlightColor } = useContext(ThemeContext)
  return (
    <Box key={label}>
      <Box display="flex" alignItems="flex-end">
        <Text>{label}</Text>
        {infoText && <InfoTooltip infoText={infoText} />}
      </Box>
      <Slider value={value} onChange={onChange} min={min} max={max} step={step}>
        <SliderTrack>
          <SliderFilledTrack />
        </SliderTrack>
        <Tooltip bg={highlightColor} label={value.toFixed(1)}>
          <SliderThumb bg="white" />
        </Tooltip>
      </Slider>
    </Box>
  )
}

export interface EnableSectionProps {
  label: string
  value: boolean | number
  onChange: () => void
  infoText?: string
  children: React.ReactNode
}

export const EnableSection = (props: EnableSectionProps) => {
  const { value, onChange, label, infoText, children } = props
  return (
    <Box paddingTop={2} key={label}>
      <Box display="flex" justifyContent="space-between" paddingBottom={2}>
        <Box display="flex" alignItems="center">
          <Text>{label}</Text>
          {infoText && <InfoTooltip infoText={infoText} />}
        </Box>
        <Switch isChecked={!!value} onChange={onChange} />
      </Box>
      <Collapse in={!!value} animateOpacity>
        <Box paddingLeft={4} paddingTop={2} paddingBottom={2}>
          {children}
        </Box>
      </Collapse>
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

export interface TagPanelProps {
  tags: string[]
  filter: typeof initialFilter
  setFilter: any
  highlightColor: string
  mode: string
}

export const TagPanel = (props: TagPanelProps) => {
  const { filter, setFilter, tags, highlightColor, mode } = props
  const tagArray = tags.map((tag) => {
    return { value: tag, label: tag }
  })

  const currentTags = mode === 'blacklist' ? 'tagsBlacklist' : 'tagsWhitelist'
  const [selectedItems, setSelectedItems] = useState<typeof tagArray>(
    filter[currentTags].map((tag) => {
      return { value: tag, label: tag }
    }),
  )

  return (
    <CUIAutoComplete
      items={tagArray}
      label={'Add tag to ' + mode}
      placeholder=" "
      onCreateItem={(item) => null}
      disableCreateItem={true}
      selectedItems={selectedItems}
      onSelectedItemsChange={(changes) => {
        if (changes.selectedItems) {
          setSelectedItems(changes.selectedItems)
          setFilter({ ...filter, [currentTags]: changes.selectedItems.map((item) => item.value) })
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
        rounded: 'full',
        bg: highlightColor,
        height: 8,
        paddingLeft: 4,
        fontWeight: 'bold',
      }}
      hideToggleButton
      itemRenderer={(selected) => selected.label}
    />
  )
}

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
