import React from 'react'
import { Text, Flex, IconButton } from '@chakra-ui/react'
import {
  BiAlignJustify,
  BiAlignLeft,
  BiAlignMiddle,
  BiAlignRight,
  BiFont,
  BiRightIndent,
} from 'react-icons/bi'
import { ChevronLeftIcon, ChevronRightIcon } from '@chakra-ui/icons'

export interface ToolbarProps {
  setJustification: any
  justification: number
  setIndent: any
  setFont: any
}

export const Toolbar = (props: ToolbarProps) => {
  const { setJustification, setIndent, setFont, justification } = props
  return (
    <Flex py={3} alignItems="center" justifyContent="space-between" pr={4}>
      <Flex>
        <IconButton variant="ghost" icon={<ChevronLeftIcon />} aria-label="Previous node" />
        <IconButton variant="ghost" icon={<ChevronRightIcon />} aria-label="Previous node" />
      </Flex>
      <Flex>
        <IconButton
          variant="ghost"
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
        <IconButton
          variant="ghost"
          aria-label="Indent Text"
          icon={<BiRightIndent />}
          onClick={() => {
            setIndent((curr: number) => (curr ? 0 : 1))
          }}
        />
        <IconButton
          variant="ghost"
          aria-label="Change font"
          icon={<BiFont />}
          onClick={() => {
            setFont((curr: string) => (curr === 'sans serif' ? 'serif' : 'sans serif'))
          }}
        />
      </Flex>
    </Flex>
  )
}
