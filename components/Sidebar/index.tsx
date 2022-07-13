import React, { useContext, useEffect, useRef, useState } from 'react'

import { Toolbar } from './Toolbar'
import { TagBar } from './TagBar'
import { Note } from './Note'
import { Title } from './Title'

import { VStack, Flex, Box, IconButton } from '@chakra-ui/react'
import { Collapse } from './Collapse'
import { Scrollbars } from 'react-custom-scrollbars-2'
import {
  ChevronLeftIcon,
  ChevronRightIcon,
  CloseIcon,
  HamburgerIcon,
  ViewIcon,
  ViewOffIcon,
} from '@chakra-ui/icons'
import { BiDotsVerticalRounded, BiFile, BiNetworkChart } from 'react-icons/bi'

import { GraphData, NodeObject, LinkObject } from 'force-graph'
import { OrgRoamNode } from '../../api'
import { ThemeContext } from '../../util/themecontext'
import { LinksByNodeId, NodeByCite, NodeById, Scope } from '../../pages/index'
import { Resizable } from 're-resizable'
import { usePersistantState } from '../../util/persistant-state'
import { initialFilter, TagColors } from '../config'

export interface SidebarProps {
  isOpen: boolean
  onClose: any
  onOpen: any
  nodeById: NodeById
  previewNode: NodeObject
  setPreviewNode: any
  linksByNodeId: LinksByNodeId
  nodeByCite: NodeByCite
  setSidebarHighlightedNode: any
  canUndo: any
  canRedo: any
  resetPreviewNode: any
  previousPreviewNode: any
  nextPreviewNode: any
  openContextMenu: any
  scope: Scope
  setScope: any
  windowWidth: number
  filter: typeof initialFilter
  setFilter: any
  tagColors: TagColors
  setTagColors: any
  macros?: { [key: string]: string }
  attachDir: string
  useInheritance: boolean
}

const Sidebar = (props: SidebarProps) => {
  const {
    isOpen,
    onOpen,
    onClose,
    previewNode,
    setPreviewNode,
    nodeById,
    linksByNodeId,
    nodeByCite,
    setSidebarHighlightedNode,
    canUndo,
    canRedo,
    resetPreviewNode,
    previousPreviewNode,
    nextPreviewNode,
    openContextMenu,
    scope,
    setScope,
    windowWidth,
    filter,
    setFilter,
    tagColors,
    setTagColors,
    macros,
    attachDir,
    useInheritance,
  } = props

  const { highlightColor } = useContext(ThemeContext)
  const [previewRoamNode, setPreviewRoamNode] = useState<OrgRoamNode | undefined>()
  const [sidebarWidth, setSidebarWidth] = usePersistantState<number>('sidebarWidth', 400)

  useEffect(() => {
    if (!previewNode?.id) {
      onClose()
      return
    }
    onOpen()
    setPreviewRoamNode(previewNode as OrgRoamNode)
  }, [previewNode?.id])

  const [justification, setJustification] = usePersistantState('justification', 1)
  const [outline, setOutline] = usePersistantState('outline', false)
  const justificationList = ['justify', 'start', 'end', 'center']
  const [font, setFont] = useState('sans serif')
  const [indent, setIndent] = useState(0)
  const [collapse, setCollapse] = useState(false)
  //maybe want to close it when clicking outside, but not sure
  //const outsideClickRef = useRef();
  return (
    <Collapse
      animateOpacity={false}
      dimension="width"
      in={isOpen}
      //style={{ position: 'relative' }}
      unmountOnExit
      startingSize={0}
      style={{ height: '100vh' }}
    >
      <Resizable
        size={{ height: '100vh', width: sidebarWidth }}
        onResizeStop={(e, direction, ref, d) => {
          setSidebarWidth((curr: number) => curr + d.width)
        }}
        enable={{
          top: false,
          right: false,
          bottom: false,
          left: true,
          topRight: false,
          bottomRight: false,
          bottomLeft: false,
          topLeft: false,
        }}
        minWidth="220px"
        maxWidth={windowWidth - 200}
      >
        <Flex flexDir="column" h="100vh" pl={2} color="black" bg="alt.100" width="100%">
          <Flex
            //whiteSpace="nowrap"
            // overflow="hidden"
            // textOverflow="ellipsis"
            pl={2}
            alignItems="center"
            color="black"
            width="100%"
          >
            <Flex pt={1} flexShrink={0}>
              <Toolbar
                {...{
                  setJustification,
                  setIndent,
                  setFont,
                  justification,
                  setPreviewNode,
                  canUndo,
                  canRedo,
                  resetPreviewNode,
                  previousPreviewNode,
                  nextPreviewNode,
                  outline,
                  setOutline,
                  collapse,
                  setCollapse,
                }}
              />
            </Flex>
            <Flex
              whiteSpace="nowrap"
              textOverflow="ellipsis"
              overflow="hidden"
              onContextMenu={(e) => {
                e.preventDefault()
                openContextMenu(previewNode, e)
              }}
            ></Flex>
            <Flex flexDir="row" ml="auto">
              <IconButton
                // eslint-disable-next-line react/jsx-no-undef
                m={1}
                icon={<BiDotsVerticalRounded />}
                aria-label="Options"
                variant="subtle"
                onClick={(e) => {
                  openContextMenu(previewNode, e, {
                    left: undefined,
                    top: 12,
                    right: -windowWidth + 20,
                    bottom: undefined,
                  })
                }}
              />
            </Flex>
          </Flex>
          <Scrollbars
            //autoHeight
            //autoHeightMax={600}
            autoHide
            renderThumbVertical={({ style, ...props }) => (
              <Box
                style={{
                  ...style,
                  borderRadius: 0,
                  // backgroundColor: highlightColor,
                }}
                //color="alt.100"
                {...props}
              />
            )}
          >
            {previewRoamNode && (
              <VStack
                flexGrow={1}
                // overflowY="scroll"
                alignItems="left"
                bg="alt.100"
                paddingLeft={4}
              >
                <Title previewNode={previewRoamNode} />
                <TagBar
                  {...{ filter, setFilter, tagColors, setTagColors, openContextMenu, previewNode }}
                />
                <Note
                  {...{
                    setPreviewNode,
                    previewNode,
                    nodeById,
                    nodeByCite,
                    setSidebarHighlightedNode,
                    justification,
                    justificationList,
                    linksByNodeId,
                    openContextMenu,
                    outline,
                    setOutline,
                    collapse,
                    macros,
                    attachDir,
                    useInheritance,
                  }}
                />
              </VStack>
            )}
          </Scrollbars>
        </Flex>
      </Resizable>
    </Collapse>
  )
}

export default Sidebar
