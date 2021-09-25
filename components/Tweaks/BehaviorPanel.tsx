import { ChevronDownIcon } from '@chakra-ui/icons'
import {
  Button,
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
import { initialBehavior, initialMouse } from '../config'
import { InfoTooltip } from './InfoTooltip'
import { SliderWithInfo } from './SliderWithInfo'

export interface BehaviorPanelProps {
  behavior: typeof initialBehavior
  setBehavior: any
  mouse: typeof initialMouse
  setMouse: any
}
export const BehaviorPanel = (props: BehaviorPanelProps) => {
  const { behavior, setBehavior, mouse, setMouse } = props
  return (
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
          <MenuButton as={Button} rightIcon={<ChevronDownIcon />} colorScheme="" color="black">
            <Text>
              {mouse.local ? mouse.local[0]!.toUpperCase() + mouse.local!.slice(1) : 'Never'}
            </Text>
          </MenuButton>
          <Portal>
            {' '}
            <MenuList zIndex="popover" bgColor="gray.200">
              <MenuItem onClick={() => setMouse({ ...mouse, local: '' })}>Never</MenuItem>
              <MenuItem onClick={() => setMouse({ ...mouse, local: 'click' })}>Click</MenuItem>
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
          <MenuButton as={Button} rightIcon={<ChevronDownIcon />} colorScheme="" color="black">
            <Text>
              {mouse.follow ? mouse.follow[0]!.toUpperCase() + mouse.follow!.slice(1) : 'Never'}
            </Text>
          </MenuButton>
          <Portal>
            {' '}
            <MenuList bgColor="gray.200" zIndex="popover">
              <MenuItem onClick={() => setMouse({ ...mouse, follow: '' })}>Never</MenuItem>
              <MenuItem onClick={() => setMouse({ ...mouse, follow: 'click' })}>Click</MenuItem>
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
          <MenuButton as={Button} rightIcon={<ChevronDownIcon />} colorScheme="" color="black">
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
          <MenuButton as={Button} rightIcon={<ChevronDownIcon />} colorScheme="" color="black">
            <Text>{behavior.localSame === 'add' ? 'Add' : 'Replace'}</Text>
          </MenuButton>
          <Portal>
            {' '}
            <MenuList bgColor="gray.200" zIndex="popover">
              <MenuItem onClick={() => setBehavior({ ...behavior, localSame: 'replace' })}>
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
  )
}
