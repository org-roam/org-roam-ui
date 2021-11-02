import { Box, Collapse, Flex, IconButton } from '@chakra-ui/react'
import React, { JSXElementConstructor, ReactChild, ReactElement, ReactNode, useState } from 'react'
import { BiCaretDownCircle, BiChevronDownCircle, BiCircle } from 'react-icons/bi'
import { ComponentLike, ComponentPropsWithoutNode } from 'rehype-react'
import { VscCircleFilled, VscCircleOutline } from 'react-icons/vsc'
import { ChevronDownIcon, ChevronLeftIcon, ChevronRightIcon, ChevronUpIcon } from '@chakra-ui/icons'

export interface SectionProps {
  children: any
  className: string
  outline: boolean
}

export const Section = (props: SectionProps) => {
  const {
    children,
    className, // outline
    outline,
  } = props
  const [open, setOpen] = useState(true)
  if (className === 'h0Wrapper headingWrapper') {
    return <Box className="preHeadingContent"> {children}</Box>
  }
  const kids = children as ReactChild[]
  return (
    <Box className={'sec'}>
      <Box display="block">
        <Flex className="headingFlex" alignItems="baseline">
          {open && kids.length > 0 ? (
            <>
              <IconButton
                className="viewerHeadingButton"
                _focus={{}}
                _active={{}}
                aria-label="Expand heading"
                //mr={1}
                size="xs"
                variant="subtle"
                icon={<ChevronUpIcon />}
                onClick={() => setOpen(!open)}
                height={2}
                width={2}
              />
              <IconButton
                className="outlineHeadingButton"
                _focus={{}}
                _active={{}}
                aria-label="Expand heading"
                //mr={1}
                size="xs"
                variant="subtle"
                icon={<VscCircleOutline />}
                onClick={() => setOpen(!open)}
                height={2}
                width={2}
              />
            </>
          ) : (
            <>
              <IconButton
                className="viewerHeadingButton"
                _active={{}}
                _focus={{}}
                aria-label="Collapse heading"
                //mr={1}
                height={2}
                width={2}
                size="xs"
                variant="subtle"
                icon={<ChevronDownIcon />}
                onClick={() => setOpen(!open)}
              />
              <IconButton
                className="outlineHeadingButton"
                _active={{}}
                _focus={{}}
                aria-label="Collapse heading"
                //mr={1}
                height={2}
                width={2}
                size="xs"
                variant="subtle"
                icon={<VscCircleFilled />}
                onClick={() => setOpen(!open)}
              />
            </>
          )}
          {kids[0]}
        </Flex>
      </Box>
      {open && <Box className="sectionContent">{kids.slice(1)}</Box>}
    </Box>
  )
}
