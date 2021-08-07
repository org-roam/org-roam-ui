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

import React, { useState, useContext, useEffect } from 'react'
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
  const [showTweaks, setShowTweaks] = useState(true)
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
                <Flex justifyContent="space-between">
                  <Text>Orphans</Text>
                  <Switch
                    onChange={() => {
                      setFilter({ ...filter, orphans: !filter.orphans })
                    }}
                    isChecked={filter.orphans}
                  ></Switch>
                </Flex>
                <Flex justifyContent="space-between">
                  <Text>Link nodes with parent file</Text>
                  <Switch
                    onChange={() => {
                      setFilter({ ...filter, parents: !filter.parents })
                    }}
                    isChecked={filter.parents}
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
                  <SliderWithInfo
                    label="Strength"
                    value={physics.gravity * 10}
                    onChange={(v) => setPhysics({ ...physics, gravity: v / 10 })}
                  />
                </EnableSection>
                <SliderWithInfo
                  value={-physics.charge / 100}
                  onChange={(value) => setPhysics({ ...physics, charge: -100 * value })}
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
                    onChange={(value) => setPhysics({ ...physics, collisionStrength: value * 5 })}
                    label="Collision Radius"
                    infoText="Easy with this one, high values can lead to a real jiggly mess"
                  />
                </EnableSection>
                <SliderWithInfo
                  value={physics.linkStrength * 5}
                  onChange={(value) => setPhysics({ ...physics, linkStrength: value / 5 })}
                  label="Link Force"
                />
                <SliderWithInfo
                  label="Link Iterations"
                  value={physics.linkIts}
                  onChange={(value) => setPhysics({ ...physics, linkIts: value })}
                  min={0}
                  max={6}
                  step={1}
                  infoText="How many links down the line the physics of a single node affects (Slow)"
                />
                <SliderWithInfo
                  label="Viscosity"
                  value={physics.velocityDecay * 10}
                  onChange={(value) => setPhysics({ ...physics, velocityDecay: value / 10 })}
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
                          onChange={(value) => setPhysics({ ...physics, alphaDecay: value / 50 })}
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
                            onChange={(v) => setPhysics({ ...physics, centeringStrength: v })}
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
                            <Menu placement="right" closeOnSelect={false} matchWidth>
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
                            <Menu placement="right">
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
                            <Menu placement="right">
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
                            visuals={visuals}
                            setVisuals={setVisuals}
                            value="linkHighlight"
                            visValue={visuals.linkHighlight}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Node highlight"
                            visuals={visuals}
                            setVisuals={setVisuals}
                            value="nodeHighlight"
                            visValue={visuals.nodeHighlight}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Background"
                            visuals={visuals}
                            setVisuals={setVisuals}
                            value="backgroundColor"
                            visValue={visuals.backgroundColor}
                          />
                          <ColorMenu
                            colorList={colorList}
                            label="Emacs node"
                            visuals={visuals}
                            setVisuals={setVisuals}
                            value="emacsNodeColor"
                            visValue={visuals.emacsNodeColor}
                          />
                        </Box>
                      </VStack>
                    </AccordionPanel>
                  </AccordionItem>
                </Accordion>
                <VStack
                  spacing={2}
                  justifyContent="flex-start"
                  divider={<StackDivider borderColor="gray.500" />}
                  align="stretch"
                  paddingLeft={7}
                  color="gray.800"
                >
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
                        onChange={(value) => setVisuals({ ...visuals, nodeResolution: value })}
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
                    label="Dash cite links"
                    infoText="Add dashes to citation links made with org-roam-bibtex"
                    value={visuals.citeDashes}
                    onChange={() => setVisuals({ ...visuals, citeDashes: !visuals.citeDashes })}
                  >
                    <SliderWithInfo
                      label="Dash length"
                      value={visuals.citeDashLength / 10}
                      onChange={(value) => setVisuals({ ...visuals, citeDashLength: value * 10 })}
                    />
                    <SliderWithInfo
                      label="Gap length"
                      value={visuals.citeGapLength / 5}
                      onChange={(value) => setVisuals({ ...visuals, citeGapLength: value * 5 })}
                    />
                  </EnableSection>
                  <ColorMenu
                    colorList={colorList}
                    label="Citation node color"
                    visuals={visuals}
                    setVisuals={setVisuals}
                    value={'citeNodeColor'}
                    visValue={visuals.citeNodeColor}
                  />
                  <ColorMenu
                    colorList={colorList}
                    label="Citation link color"
                    visuals={visuals}
                    setVisuals={setVisuals}
                    value={'citeLinkColor'}
                    visValue={visuals.citeLinkColor}
                  />
                  <ColorMenu
                    colorList={colorList}
                    label="Reference link highlight"
                    visuals={visuals}
                    setVisuals={setVisuals}
                    value={'citeLinkHighlightColor'}
                    visValue={visuals.citeLinkHighlightColor}
                  />
                  <EnableSection
                    label="Dash ref links"
                    infoText="Add dashes to citation links, whose target has a note, made with org-roam-bibtex"
                    value={visuals.refDashes}
                    onChange={() => setVisuals({ ...visuals, refDashes: !visuals.refDashes })}
                  >
                    <SliderWithInfo
                      label="Dash length"
                      value={visuals.refDashLength / 10}
                      onChange={(value) => setVisuals({ ...visuals, refDashLength: value * 10 })}
                    />
                    <SliderWithInfo
                      label="Gap length"
                      value={visuals.refGapLength / 5}
                      onChange={(value) => setVisuals({ ...visuals, refGapLength: value * 5 })}
                    />
                  </EnableSection>
                  <ColorMenu
                    colorList={colorList}
                    label="Reference node color"
                    visuals={visuals}
                    setVisuals={setVisuals}
                    value={'refNodeColor'}
                    visValue={visuals.refNodeColor}
                  />
                  <ColorMenu
                    colorList={colorList}
                    label="Reference link color"
                    visuals={visuals}
                    setVisuals={setVisuals}
                    value={'refLinkColor'}
                    visValue={visuals.refLinkColor}
                  />
                  <ColorMenu
                    colorList={colorList}
                    label="Reference link highlight"
                    visuals={visuals}
                    setVisuals={setVisuals}
                    value={'refLinkHighlightColor'}
                    visValue={visuals.refLinkHighlightColor}
                  />
                  <Box>
                    <Flex alignItems="center" justifyContent="space-between">
                      <Text>Labels</Text>
                      <Menu placement="right">
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
                    <Collapse in={visuals.labels > 0} animateOpacity>
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
                          label="Maximum label characters"
                          value={visuals.labelLength}
                          min={10}
                          max={100}
                          step={1}
                          onChange={(value) => setVisuals({ ...visuals, labelLength: value })}
                        />
                        <ColorMenu
                          colorList={colorList}
                          label="Text"
                          visuals={visuals}
                          setVisuals={setVisuals}
                          value="labelTextColor"
                          visValue={visuals.labelTextColor}
                        />
                        <ColorMenu
                          colorList={colorList}
                          label="Background"
                          visuals={visuals}
                          setVisuals={setVisuals}
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
                    </Collapse>
                  </Box>
                  <EnableSection
                    label="Link arrows"
                    value={visuals.arrows}
                    onChange={() => setVisuals({ ...visuals, arrows: !visuals.arrows })}
                  >
                    <SliderWithInfo
                      label="Arrow size"
                      value={visuals.arrowsLength / 10}
                      onChange={(value) => setVisuals({ ...visuals, arrowsLength: 10 * value })}
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
                      visuals={visuals}
                      setVisuals={setVisuals}
                      value="arrowsColor"
                      visValue={visuals.arrowsColor}
                    />
                  </EnableSection>
                  <EnableSection
                    label="Directional Particles"
                    value={visuals.particles}
                    onChange={() => setVisuals({ ...visuals, particles: !visuals.particles })}
                  >
                    <SliderWithInfo
                      label="Particle Number"
                      value={visuals.particlesNumber}
                      max={5}
                      step={1}
                      onChange={(value) => setVisuals({ ...visuals, particlesNumber: value })}
                    />
                    <SliderWithInfo
                      label="Particle Size"
                      value={visuals.particlesWidth}
                      onChange={(value) => setVisuals({ ...visuals, particlesWidth: value })}
                    />
                  </EnableSection>
                  <EnableSection
                    label="Highlight"
                    onChange={() => setVisuals({ ...visuals, highlight: !visuals.highlight })}
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
                        onChange={(value) => setVisuals({ ...visuals, highlightLinkSize: value })}
                      />
                      <SliderWithInfo
                        label="Highlight Node Size"
                        value={visuals.highlightNodeSize}
                        onChange={(value) => setVisuals({ ...visuals, highlightNodeSize: value })}
                      />
                      <SliderWithInfo
                        min={0}
                        max={1}
                        label="Highlight Fade"
                        value={visuals.highlightFade}
                        onChange={(value) => setVisuals({ ...visuals, highlightFade: value })}
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
                </VStack>
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
                  <Menu placement="right">
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
                  <Menu placement="right">
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
                  <Menu placement="right">
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
                {/* <Flex alignItems="center" justifyContent="space-between">
                      <Flex>
                        <Text>Follow local graph</Text>
                        <InfoTooltip infoText="When in local mode and opening a node that already exists in Emacs, should I add that local graph or open the new one?" />
                      </Flex>
                      <Menu placement="right">
                        <MenuButton
                          as={Button}
                          rightIcon={<ChevronDownIcon />}
                          colorScheme=""
                          color="black"
                        >
                          <Text>{behavior.localSame === 'add' ? 'Add' : 'New'}</Text>
                        </MenuButton>
                        <Portal>
                          {' '}
                          <MenuList bgColor="gray.200" zIndex="popover">
                            <MenuItem
                              onClick={() => setBehavior({ ...behavior, localSame: 'new' })}
                            >
                              Open that nodes graph
                            </MenuItem>
                            <MenuItem
                              onClick={() => setBehavior({ ...behavior, localSame: 'add' })}
                            >
                              Add node to local graph
                            </MenuItem>
                          </MenuList>
                        </Portal>
                      </Menu>
                    </Flex> */}
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
    <Box>
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
    <Box paddingTop={2}>
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
    <Menu placement="right">
      <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
        {displayValue}
      </MenuButton>
      <Portal>
        {' '}
        <MenuList zIndex="popover">
          {textArray.map((option, i) => {
            ; <MenuItem onClick={onClickArray[i]}> {option} </MenuItem>
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
  visuals: typeof initialVisuals
  visValue: string
  setVisuals?: any
}

export const ColorMenu = (props: ColorMenuProps) => {
  const { label, colorList, value, visuals, visValue, setVisuals } = props
  return (
    <Flex alignItems="center" justifyContent="space-between">
      <Text>{label}</Text>
      <Menu placement="right">
        <MenuButton as={Button} colorScheme="" color="black" rightIcon={<ChevronDownIcon />}>
          {<Box bgColor={visValue} borderRadius="sm" height={6} width={6}></Box>}
        </MenuButton>
        <Portal>
          {' '}
          <MenuList minW={10} zIndex="popover" bgColor="gray.200">
            <MenuItem
              onClick={() => setVisuals({ ...visuals, [value]: '' })}
              justifyContent="space-between"
              alignItems="center"
              display="flex"
            >
              <Box height={6} width={6}></Box>
            </MenuItem>
            {colorList.map((color: string) => (
              <MenuItem
                key={color}
                onClick={() =>
                  setVisuals({
                    ...visuals,
                    [value]: color,
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
