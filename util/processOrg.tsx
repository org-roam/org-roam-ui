import unified from 'unified'
//import createStream from 'unified-stream'
import uniorgParse from 'uniorg-parse'
import uniorg2rehype from 'uniorg-rehype'
//import highlight from 'rehype-highlight'
import katex from 'rehype-katex'
import 'katex/dist/katex.css'
import rehype2react from 'rehype-react'

import { PreviewLink } from '../components/Sidebar/Link'
import { NodeById } from '../pages'
import React from 'react'

export interface ProcessedOrgProps {
  nodeById: NodeById
  previewNode: any
  setPreviewNode: any
  getText: any
  previewText: any
}

export const ProcessedOrg = (props: ProcessedOrgProps) => {
  const { nodeById, previewNode, setPreviewNode, getText, previewText } = props
  console.log(previewText)
  const processor = unified()
    .use(uniorgParse)
    .use(uniorg2rehype)
    .use(katex)
    .use(rehype2react, {
      createElement: React.createElement,
      // eslint-disable-next-line react/display-name
      components: {
        a: ({ props, children, href }) => (
          <PreviewLink
            getText={getText}
            nodeById={nodeById}
            previewNode={previewNode}
            setPreviewNode={setPreviewNode}
            {...{ children, href }}
          />
        ),
      },
    })

  return <div>{processor.processSync(previewText).result}</div>
}
