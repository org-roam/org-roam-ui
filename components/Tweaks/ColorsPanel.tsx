import { ArrowRightIcon, ChevronDownIcon, RepeatIcon } from '@chakra-ui/icons'
import {
  Text,
  Box,
  Flex,
  IconButton,
  StackDivider,
  Tooltip,
  VStack,
  MenuButton,
  Menu,
  Portal,
  MenuList,
  MenuOptionGroup,
  MenuItemOption,
  Button,
  MenuItem,
} from '@chakra-ui/react'
import React from 'react'
import { ColorMenu } from './ColorMenu'
import { colorList, initialVisuals } from '../config'

export interface ColorsPanelProps {
  visuals: typeof initialVisuals
  setVisualsCallback: any
  highlightColor: string
  setHighlightColor: any
}

export const ColorsPanel = (props: ColorsPanelProps) => {
  const { visuals, setVisualsCallback, highlightColor, setHighlightColor } = props

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
          <Text>Nodes</Text>
          <Tooltip label="Shuffle node colors">
            <IconButton
              aria-label="Shuffle node colors"
              size="sm"
              icon={<RepeatIcon />}
              variant="ghost"
              onClick={() => {
                const arr = visuals.nodeColorScheme ?? []
                setVisualsCallback({
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
                setVisualsCallback({
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
                  <Box key={color} bgColor={color} flex="1 1 8px" borderRadius="2xl"></Box>
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
                    setVisualsCallback({ ...visuals, nodeColorScheme: colors })
                  }}
                >
                  {colorList.map((color) => (
                    <MenuItemOption
                      key={color}
                      isChecked={visuals.nodeColorScheme.some((c) => c === color)}
                      value={color}
                      isDisabled={
                        visuals.nodeColorScheme.length === 1 && visuals.nodeColorScheme[0] === color
                      }
                    >
                      <Box justifyContent="space-between" alignItems="center" display="flex">
                        <Box bgColor={color} borderRadius="sm" height={6} width={6}></Box>
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
            <MenuButton as={Button} colorScheme="" color="black" rightIcon={<ChevronDownIcon />}>
              <Box>
                {visuals.linkColorScheme ? (
                  <Box
                    bgColor={visuals.linkColorScheme}
                    borderRadius="sm"
                    height={6}
                    width={6}
                  ></Box>
                ) : (
                  <Flex height={6} width={6} flexDirection="column" flexWrap="wrap">
                    {visuals.nodeColorScheme.map((color) => (
                      <Box key={color} bgColor={color} flex="1 1 8px" borderRadius="2xl"></Box>
                    ))}
                  </Flex>
                )}
              </Box>
            </MenuButton>
            <Portal>
              {' '}
              <MenuList minW={10} zIndex="popover" bgColor="gray.200">
                <MenuItem
                  onClick={() => setVisualsCallback({ ...visuals, linkColorScheme: '' })}
                  justifyContent="space-between"
                  alignItems="center"
                  display="flex"
                >
                  <Flex height={6} width={6} flexDirection="column" flexWrap="wrap">
                    {visuals.nodeColorScheme.map((color) => (
                      <Box key={color} bgColor={color} flex="1 1 8px" borderRadius="2xl"></Box>
                    ))}
                  </Flex>
                </MenuItem>
                {colorList.map((color) => (
                  <MenuItem
                    key={color}
                    onClick={() =>
                      setVisualsCallback({
                        ...visuals,
                        linkColorScheme: color,
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
        <Flex alignItems="center" justifyContent="space-between">
          <Text>Accent</Text>
          <Menu isLazy placement="right">
            <MenuButton as={Button} colorScheme="" color="black" rightIcon={<ChevronDownIcon />}>
              {<Box bgColor={highlightColor} borderRadius="sm" height={6} width={6}></Box>}
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
                    <Box bgColor={color} borderRadius="sm" height={6} width={6}></Box>
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
  )
}
