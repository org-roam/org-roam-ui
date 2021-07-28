import {
  CloseIcon,
  RepeatClockIcon,
  ChevronDownIcon,
  SettingsIcon,
  InfoOutlineIcon,
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
} from '@chakra-ui/react'
import React, { useState, useContext } from 'react'
import Scrollbars from 'react-custom-scrollbars-2'
import { initialPhysics, initialFilter, initialVisuals } from './config'

import { ThemeContext } from '../pages/themecontext'

export interface TweakProps {
  physics: typeof initialPhysics
  setPhysics: any
  threeDim: boolean
  setThreeDim: (newValue: boolean) => void
  filter: typeof initialFilter
  setFilter: any
  visuals: typeof initialVisuals
  setVisuals: any
}

export const Tweaks = (props: TweakProps) => {
  const { physics, setPhysics, threeDim, setThreeDim, filter, setFilter, visuals, setVisuals } =
    props
  const [showTweaks, setShowTweaks] = useState(true)
  const { highlightColor, setHighlightColor } = useContext(ThemeContext)

  return (
    <>
      <Box
        position="relative"
        zIndex="overlay"
        marginTop={10}
        marginLeft={10}
        display={showTweaks ? 'none' : 'block'}
      >
        <IconButton
          aria-label="Settings"
          icon={<SettingsIcon />}
          onClick={() => setShowTweaks(true)}
        />
      </Box>
      <Collapse in={showTweaks} animateOpacity>
        <Box
          bg="alt.100"
          w={300}
          marginTop={10}
          marginLeft={10}
          borderRadius="xl"
          maxH={650}
          paddingBottom={5}
          zIndex={300}
          position="relative"
          boxShadow="xl"
        >
          <Box display="flex" justifyContent="space-between" alignItems="center">
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
                  onClick={() => setPhysics(initialPhysics)}
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
            autoHeightMax={600}
            autoHide
            renderThumbVertical={({ style, ...props }) => (
              <Box
                {...props}
                style={{
                  ...style,
                  borderRadius: 10,
                }}
                bg={highlightColor + '.500'}
              />
            )}
          >
            <Accordion allowMultiple allowToggle color="black" paddingRight={2}>
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
                  </VStack>
                </AccordionPanel>
              </AccordionItem>
              <AccordionItem>
                <AccordionButton display="flex" justifyContent="space-between">
                  <Box display="flex">
                    <AccordionIcon marginRight={2} />
                    <Heading size="sm">Physics</Heading>
                  </Box>
                  <Switch
                    id="physicsOn"
                    onChange={() => setPhysics({ ...physics, enabled: !physics.enabled })}
                    isChecked={physics.enabled}
                  />
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
                        onChange={(value) =>
                          setPhysics({ ...physics, collisionStrength: value * 5 })
                        }
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
                    <Accordion allowToggle>
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
                              onChange={(value) =>
                                setPhysics({ ...physics, alphaDecay: value / 50 })
                              }
                            />
                            <EnableSection
                              label="Keeps nodes centered"
                              value={physics.centering}
                              onChange={() =>
                                setPhysics({ ...physics, centering: !physics.centering })
                              }
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
                  {/* </VStack> */}
                </AccordionPanel>
              </AccordionItem>
              <AccordionItem>
                <AccordionButton>
                  <AccordionIcon marginRight={2} />
                  <Heading size="sm">Visual</Heading>
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
                    <Accordion>
                      <AccordionItem>
                        <AccordionButton>
                          <AccordionIcon marginRight={2} />
                          <Heading size="sm">Visual</Heading>
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
                            <Box>
                              <Flex alignItems="center" justifyContent="space-between">
                                <Text>Node Color Scheme</Text>
                                <Menu>
                                  <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
                                    {visuals.nodeColorScheme === 'colorful' ? 'Colorful' : 'Plain'}
                                  </MenuButton>
                                  <MenuList bgColor="gray.200">
                                    <MenuItem onClick={() => setPhysics({ ...physics, labels: 1 })}>
                                      Colorful
                                    </MenuItem>
                                    <MenuItem onClick={() => setPhysics({ ...physics, labels: 2 })}>
                                      Plain
                                    </MenuItem>
                                  </MenuList>
                                </Menu>
                              </Flex>
                              <Flex alignItems="center" justifyContent="space-between">
                                <Text>Link Color Scheme</Text>
                                <Menu>
                                  <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
                                    {visuals.linkColorScheme === 'colorful' ? 'Colorful' : 'Plain'}
                                  </MenuButton>
                                  <MenuList bgColor="gray.200">
                                    <MenuItem onClick={() => setPhysics({ ...physics, labels: 1 })}>
                                      Colorful
                                    </MenuItem>
                                    <MenuItem onClick={() => setPhysics({ ...physics, labels: 2 })}>
                                      Plain
                                    </MenuItem>
                                  </MenuList>
                                </Menu>
                              </Flex>
                              <Flex alignItems="center" justifyContent="space-between">
                                <Text>Highlight color</Text>
                                <Menu>
                                  <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
                                    {highlightColor[0]!.toUpperCase() + highlightColor!.slice(1)}
                                  </MenuButton>
                                  <MenuList bgColor="gray.200" width={50}>
                                    {[
                                      'red',
                                      'orange',
                                      'yellow',
                                      'green',
                                      'cyan',
                                      'blue',
                                      'pink',
                                      'purple',
                                    ].map((color) => (
                                      <MenuItem
                                        key={color}
                                        onClick={() => setHighlightColor(color)}
                                        justifyContent="space-between"
                                        alignItems="center"
                                        display="flex"
                                      >
                                        <Text>{color[0]!.toUpperCase() + color!.slice(1)}</Text>
                                        <Box
                                          bgColor={color + '.500'}
                                          borderRadius="sm"
                                          height={6}
                                          width={6}
                                        ></Box>
                                      </MenuItem>
                                    ))}
                                  </MenuList>
                                </Menu>
                              </Flex>
                            </Box>
                          </VStack>
                        </AccordionPanel>
                      </AccordionItem>
                    </Accordion>
                    <SliderWithInfo
                      label="Node size"
                      value={physics.nodeRel}
                      onChange={(value) => setPhysics({ ...physics, nodeRel: value })}
                    />
                    {threeDim && (
                      <>
                        <SliderWithInfo
                          label="Node opacity"
                          value={physics.nodeOpacity}
                          min={0}
                          max={1}
                          onChange={(value) => setPhysics({ ...physics, nodeOpacity: value })}
                        />
                        <SliderWithInfo
                          label="Node resolution"
                          value={physics.nodeResolution}
                          min={5}
                          max={32}
                          step={1}
                          onChange={(value) => setPhysics({ ...physics, nodeResolution: value })}
                        />
                      </>
                    )}
                    <SliderWithInfo
                      label="Link width"
                      value={physics.linkWidth}
                      onChange={(value) => setPhysics({ ...physics, linkWidth: value })}
                    />
                    {threeDim && (
                      <SliderWithInfo
                        label="Link opacity"
                        min={0}
                        max={1}
                        value={physics.linkOpacity}
                        onChange={(value) => setPhysics({ ...physics, linkOpacity: value })}
                      />
                    )}
                    <Box>
                      <Flex alignItems="center" justifyContent="space-between">
                        <Text>Labels</Text>
                        <Menu>
                          <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
                            {!physics.labels
                              ? 'Never'
                              : physics.labels < 2
                              ? 'On Highlight'
                              : 'Always'}
                          </MenuButton>
                          <MenuList bgColor="gray.200">
                            <MenuItem onClick={() => setPhysics({ ...physics, labels: 0 })}>
                              Never
                            </MenuItem>
                            <MenuItem onClick={() => setPhysics({ ...physics, labels: 1 })}>
                              On Highlight
                            </MenuItem>
                            <MenuItem onClick={() => setPhysics({ ...physics, labels: 2 })}>
                              Always
                            </MenuItem>
                          </MenuList>
                        </Menu>
                      </Flex>
                      <Collapse in={physics.labels > 1} animateOpacity>
                        <Box paddingLeft={4} paddingTop={2}>
                          <SliderWithInfo
                            label="Label Appearance Scale"
                            value={physics.labelScale * 5}
                            onChange={(value) => setPhysics({ ...physics, labelScale: value / 5 })}
                          />
                        </Box>
                      </Collapse>
                    </Box>
                    <EnableSection
                      label="Directional Particles"
                      value={physics.particles}
                      onChange={() => setPhysics({ ...physics, particles: !physics.particles })}
                    >
                      <SliderWithInfo
                        label="Particle Number"
                        value={physics.particlesNumber}
                        max={5}
                        step={1}
                        onChange={(value) => setPhysics({ ...physics, particlesNumber: value })}
                      />
                      <SliderWithInfo
                        label="Particle Size"
                        value={physics.particlesWidth}
                        onChange={(value) => setPhysics({ ...physics, particlesWidth: value })}
                      />
                    </EnableSection>
                    <EnableSection
                      label="Highlight"
                      onChange={() => setPhysics({ ...physics, highlight: !physics.highlight })}
                      value={physics.highlight}
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
                          value={physics.highlightLinkSize}
                          onChange={(value) => setPhysics({ ...physics, highlightLinkSize: value })}
                        />
                        <SliderWithInfo
                          label="Highlight Node Size"
                          value={physics.highlightNodeSize}
                          onChange={(value) => setPhysics({ ...physics, highlightNodeSize: value })}
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
                            setPhysics({ ...physics, highlightAnim: !physics.highlightAnim })
                          }}
                          value={physics.highlightAnim}
                        >
                          <SliderWithInfo
                            label="Animation speed"
                            onChange={(v) => setPhysics({ ...physics, animationSpeed: v })}
                            value={physics.animationSpeed}
                            infoText="Slower speed has a chance of being buggy"
                            min={50}
                            max={1000}
                            step={10}
                          />
                          <Select
                            placeholder={physics.algorithmName}
                            onChange={(v) => {
                              setPhysics({ ...physics, algorithmName: v.target.value })
                            }}
                          >
                            {physics.algorithmOptions.map((opt: string) => (
                              <option key={opt} value={opt}>
                                {opt}
                              </option>
                            ))}
                          </Select>
                          {/* <DropDownMenu
                    displayValue={physics.algorithmName}
                    textArray={physics.algorithmOptions}
                    onClickArray={physics.algorithmOptions.map((option) =>
                      setPhysics({ ...physics, algorithmName: { option } }),
                    )}
                  /> */}
                        </EnableSection>
                      </VStack>
                    </EnableSection>
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
                  ></VStack>
                </AccordionPanel>
              </AccordionItem>
            </Accordion>
          </Scrollbars>
        </Box>
      </Collapse>
    </>
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
        <Tooltip bg={highlightColor + '.500'} label={value.toFixed(1)}>
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
    <Box>
      <Box display="flex" justifyContent="space-between">
        <Box display="flex" alignItems="center">
          <Text>{label}</Text>
          {infoText && <InfoTooltip infoText={infoText} />}
        </Box>
        <Switch isChecked={!!value} onChange={onChange} />
      </Box>
      <Collapse in={!!value} animateOpacity>
        <Box paddingLeft={4} paddingTop={2}>
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
    <Menu>
      <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
        {displayValue}
      </MenuButton>
      <MenuList>
        {textArray.map((option, i) => {
          ;<MenuItem onClick={onClickArray[i]}> {option} </MenuItem>
        })}
      </MenuList>
    </Menu>
  )
}
