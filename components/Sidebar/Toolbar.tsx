import React from 'react'
import { Text, Flex, IconButton, ButtonGroup, Tooltip } from '@chakra-ui/react'
import {
  BiAlignJustify,
  BiAlignLeft,
  BiAlignMiddle,
  BiAlignRight,
  BiFont,
  BiRightIndent,
} from 'react-icons/bi'
import { ChevronLeftIcon, ChevronRightIcon } from '@chakra-ui/icons'
import { IoIosListBox, IoMdListBox } from 'react-icons/io'
import { NodeObject } from 'force-graph'

export interface ToolbarProps {
  setJustification: any
  justification: number
  setIndent: any
  setFont: any
  setPreviewNode: any
  canUndo: any
  canRedo: any
  resetPreviewNode: any
  previousPreviewNode: any
  nextPreviewNode: any
  outline: boolean
  setOutline: any
}

export const Toolbar = (props: ToolbarProps) => {
  const {
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
  } = props
  return (
    <Flex flex="0 1 40px" pb={3} alignItems="center" justifyContent="space-between" pr={1}>
      <Flex>
        <ButtonGroup isAttached>
          <Tooltip label="Go backward">
            <IconButton
              _focus={{}}
              variant="subtle"
              icon={<ChevronLeftIcon />}
              aria-label="Previous node"
              disabled={!canUndo}
              onClick={() => previousPreviewNode()}
            />
          </Tooltip>
          <Tooltip label="Go forward">
            <IconButton
              _focus={{}}
              variant="subtle"
              icon={<ChevronRightIcon />}
              aria-label="Next node"
              disabled={!canRedo}
              onClick={() => nextPreviewNode()}
            />
          </Tooltip>
        </ButtonGroup>
      </Flex>
      <Flex>
        <Tooltip label="Toggle outline view">
          <IconButton
            variant="subtle"
            aria-label="Justify content"
            icon={outline ? <IoIosListBox /> : <IoMdListBox />}
            onClick={() => setOutline((curr: boolean) => !curr)}
          />
        </Tooltip>
        <Tooltip label="Justify content">
          <IconButton
            variant="subtle"
            aria-label="Justify content"
            icon={
              [
                <BiAlignJustify key="justify" />,
                <BiAlignLeft key="left" />,
                <BiAlignRight key="right" />,
                <BiAlignMiddle key="center" />,
              ][justification]
            }
            onClick={() => setJustification((curr: number) => (curr + 1) % 4)}
          />
        </Tooltip>
        {/* <Tooltip label="Indent trees">
          <IconButton
            variant="subtle"
            aria-label="Indent Text"
            icon={<BiRightIndent />}
            onClick={() => {
              setIndent((curr: number) => (curr ? 0 : 1))
            }}
          />
        </Tooltip>
        <Tooltip label="Switch betwwen sans and serif">
          <IconButton
            variant="subtle"
            aria-label="Change font"
            icon={<BiFont />}
            onClick={() => {
              setFont((curr: string) => (curr === 'sans serif' ? 'serif' : 'sans serif'))
            }}
          />
        </Tooltip> */}
      </Flex>
    </Flex>
  )
}
