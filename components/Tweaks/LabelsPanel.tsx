import { ChevronDownIcon } from '@chakra-ui/icons'
import {
  Box,
  Button,
  Collapse,
  Flex,
  Menu,
  MenuButton,
  MenuItem,
  MenuList,
  Portal,
  StackDivider,
  VStack,
  Text,
} from '@chakra-ui/react'
import React from 'react'
import { ColorMenu } from './ColorMenu'
import { colorList, initialVisuals } from '../config'
import { SliderWithInfo } from './SliderWithInfo'

export interface LabelsPanelProps {
  visuals: typeof initialVisuals
  setVisuals: any
}
export const LabelsPanel = (props: LabelsPanelProps) => {
  const { visuals, setVisuals } = props
  return (
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
            <MenuButton as={Button} colorScheme="" color="black" rightIcon={<ChevronDownIcon />}>
              {!visuals.labels ? 'Never' : visuals.labels < 2 ? 'On Highlight' : 'Always'}
            </MenuButton>
            <Portal>
              {' '}
              <MenuList zIndex="popover" bgColor="gray.200">
                <MenuItem onClick={() => setVisuals({ ...visuals, labels: 0 })}>Never</MenuItem>
                <MenuItem onClick={() => setVisuals({ ...visuals, labels: 1 })}>
                  On Highlight
                </MenuItem>
                <MenuItem onClick={() => setVisuals({ ...visuals, labels: 2 })}>Always</MenuItem>
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
            onChange={(value) => setVisuals({ ...visuals, labelLineSpace: value })}
          />
          <ColorMenu
            colorList={colorList}
            label="Text"
            setVisuals={setVisuals}
            value="labelTextColor"
            visValue={visuals.labelTextColor}
          />
          <ColorMenu
            colorList={colorList}
            label="Background"
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
                onChange={(value) => setVisuals({ ...visuals, labelScale: value / 5 })}
              />
            </Box>
          </Collapse>
        </VStack>
      </Box>
    </VStack>
  )
}
