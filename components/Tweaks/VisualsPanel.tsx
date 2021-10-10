import {
  Text,
  Accordion,
  AccordionButton,
  AccordionItem,
  Flex,
  VStack,
  AccordionIcon,
  AccordionPanel,
  MenuButton,
  Menu,
  Button,
  Box,
  Portal,
  MenuList,
  MenuItem,
} from '@chakra-ui/react'
import React, { useCallback } from 'react'
import { HighlightingPanel } from './HighlightingPanel'
import { ColorsPanel } from './ColorsPanel'
import { initialVisuals } from '../config'
import { NodesNLinksPanel } from './NodesNLinksPanel'
import { LabelsPanel } from './LabelsPanel'
import { CitationsPanel } from './CitationsPanel'
import { ColorMenu } from './ColorMenu'
import { ThemeSelect } from './ThemeSelect'

export interface VisualsPanelProps {
  visuals: typeof initialVisuals
  setVisuals: any
  highlightColor: string
  setHighlightColor: any
  threeDim: boolean
}

export const VisualsPanel = (props: VisualsPanelProps) => {
  const { visuals, setVisuals, highlightColor, setHighlightColor, threeDim } = props
  const setVisualsCallback = useCallback((val) => setVisuals(val), [])
  return (
    <VStack justifyContent="flex-start" align="stretch">
      <ThemeSelect />
      <Accordion allowToggle defaultIndex={[0]} paddingLeft={3}>
        <AccordionItem>
          <AccordionButton>
            <Flex justifyContent="space-between" w="100%">
              <Text>Colors</Text>
              <AccordionIcon marginRight={2} />
            </Flex>
          </AccordionButton>
          <AccordionPanel>
            <ColorsPanel
              visuals={visuals}
              setVisualsCallback={setVisualsCallback}
              highlightColor={highlightColor}
              setHighlightColor={setHighlightColor}
            />
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
            <NodesNLinksPanel
              visuals={visuals}
              setVisuals={setVisualsCallback}
              threeDim={threeDim}
            />
          </AccordionPanel>
        </AccordionItem>
        <AccordionItem>
          <AccordionButton>
            <Flex justifyContent="space-between" w="100%">
              <Text>Labels</Text>
              <AccordionIcon marginRight={2} />
            </Flex>
          </AccordionButton>
          <AccordionPanel>
            <LabelsPanel visuals={visuals} setVisuals={setVisualsCallback} />
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
            <HighlightingPanel visuals={visuals} setVisuals={setVisualsCallback} />
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
            <CitationsPanel visuals={visuals} setVisuals={setVisualsCallback} />
          </AccordionPanel>
        </AccordionItem>
      </Accordion>
    </VStack>
  )
}
function clickCallback(color: string): void {
  throw new Error('Function not implemented.')
}
