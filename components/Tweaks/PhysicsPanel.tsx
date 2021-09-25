import {
  Text,
  Accordion,
  AccordionButton,
  AccordionIcon,
  AccordionItem,
  AccordionPanel,
  Box,
  Flex,
  StackDivider,
  Switch,
  VStack,
} from '@chakra-ui/react'
import React, { useCallback } from 'react'
import { initialPhysics } from '../config'
import { EnableSection } from './EnableSection'
import { SliderWithInfo } from './SliderWithInfo'

export interface PhysicsPanelProps {
  physics: typeof initialPhysics
  setPhysics: any
}

export const PhysicsPanel = (props: PhysicsPanelProps) => {
  const { physics, setPhysics } = props
  const setPhysicsCallback = useCallback((val: number, phys: string, scale: number) => {
    setPhysics((curr: typeof initialPhysics) => {
      return { ...curr, [phys]: val / scale }
    })
  }, [])
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
    </Box>
  )
}
