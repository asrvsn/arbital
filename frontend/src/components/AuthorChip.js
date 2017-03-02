import React, { Component, PropTypes } from 'react'

const styles = {
  chip: {
    margin: 4,
  },
}

const goToAuthor = (authorId) => {
  // TODO navigate to author's page
}

const AuthorChip = (props) => {
  const { authorName, authorId } = props

  return (
    <Chip onTouchTap={e =>
            goToAuthor(authorId)
          }
          style={styles.chip}>
      <Avatar size={32}>{authorName[0]}</Avatar>
      {authorName}
    </Chip>
  )
}
