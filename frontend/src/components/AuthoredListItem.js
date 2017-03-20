import React, { Component, PropTypes } from 'react'
import { ListItem } from 'material-ui/List';

import AuthorChip from './AuthorChip'

const styles = {
  chip: {
    float: 'right'
  }
}

const AuthoredListItem = (props) => {
  const { text, authorName, authorId, onTouchTap, onAuthorTouchTap } = props

  const authorIcon = (
    <div style={styles.chip}>
      <AuthorChip
        authorName={authorName}
        authorId={authorId}
        onTouchTap={e => {
          e.preventDefault()
          e.stopPropagation()
          onAuthorTouchTap(e)
        }}
      />
    </div>
  )

  return (
    <ListItem
      primaryText={text}
      onTouchTap={e => onTouchTap(e)}
    >
      {authorIcon}
    </ListItem>
  )
}

export default AuthoredListItem
