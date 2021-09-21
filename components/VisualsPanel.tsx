import { ArrowRightIcon, ChevronDownIcon } from '@chakra-ui/icons'
import {
  Text,
  Accordion,
  AccordionButton,
  AccordionItem,
  Box,
  Flex,
  IconButton,
  Menu,
  MenuButton,
  MenuItemOption,
  MenuList,
  MenuOptionGroup,
  Portal,
  Tooltip,
  VStack,
  AccordionIcon,
  AccordionPanel,
  MenuItem,
  Collapse,
  StackDivider,
  Button,
} from '@chakra-ui/react'
import React, { useCallback } from 'react'
import { ColorMenu } from './ColorMenu'
import { EnableSection } from './EnableSection'
import { SliderWithInfo } from './SliderWithInfo'
import { HighlightingPanel } from './HighlightingPanel'
import { ColorsPanel } from './ColorsPanel'
import { colorList, initialVisuals } from './config'
import { NodesNLinksPanel } from './NodesNLinksPanel'
import { LabelsPanel } from './LabelsPanel'
import { CitationsPanel } from './CitationsPanel'

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
