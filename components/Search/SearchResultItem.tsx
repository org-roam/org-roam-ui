import { ListItem, Tag, Text, Wrap, WrapItem, Heading } from '@chakra-ui/react'
import { useContext, useMemo } from 'react'
import { ThemeContext } from '../../util/themecontext'
import { highlightWords } from './highlight'
import { NodeMatch } from './matchNodes'

type Props = {
  match: NodeMatch
  onClick: (id: string) => void
}

const EXCERPT_RADIUS = 60

// Trims a long snippet (a heading line or the note body) down to a window
// around the first matched word, so the result doesn't show the whole note.
const buildExcerpt = (text: string, words: string[]) => {
  const lower = text.toLowerCase()
  const firstIndex = words.reduce((min, word) => {
    const idx = lower.indexOf(word)
    return idx === -1 ? min : Math.min(min, idx)
  }, Infinity)
  if (!Number.isFinite(firstIndex)) {
    return text.slice(0, EXCERPT_RADIUS * 2)
  }
  const start = Math.max(0, firstIndex - EXCERPT_RADIUS)
  const end = Math.min(text.length, firstIndex + EXCERPT_RADIUS)
  const prefix = start > 0 ? '…' : ''
  const suffix = end < text.length ? '…' : ''
  return prefix + text.slice(start, end).trim() + suffix
}

export const SearchResultItem: React.FC<Props> = ({ match, onClick }) => {
  const { highlightColor, emacsTheme } = useContext(ThemeContext)
  type Theme = { [color: string]: string }
  const themeColors = emacsTheme[1] as Theme
  const { id, title } = match.node
  // Untagged nodes arrive from Emacs with tags `[null]`.
  const tags = match.node.tags?.filter((tag) => !!tag)

  const excerpt = useMemo(() => {
    if (match.category === 'title') {
      return null
    }
    return buildExcerpt(match.snippet, match.words)
  }, [match])

  return (
    <ListItem
      p={2}
      borderRadius={4}
      cursor="pointer"
      bgColor={themeColors['bg-alt']}
      _hover={{ borderColor: highlightColor, color: highlightColor, opacity: 0.8 }}
      onClick={() => onClick(id)}
    >
      <Heading size="sm">
        {match.category === 'title' ? highlightWords(title, match.words) : title}
      </Heading>
      {!!tags?.length && (
        <Wrap marginTop={2} spacingY={1}>
          {tags.map((t) => (
            <WrapItem key={t}>
              <Tag size="sm" colorScheme="blue">
                {t}
              </Tag>
            </WrapItem>
          ))}
        </Wrap>
      )}
      {excerpt && (
        <Text fontSize="sm" opacity={0.8} marginTop={2} noOfLines={3}>
          {match.category === 'heading' && (
            <Text as="span" fontWeight="semibold" opacity={0.7}>
              {'# '}
            </Text>
          )}
          {highlightWords(excerpt, match.words)}
        </Text>
      )}
    </ListItem>
  )
}
