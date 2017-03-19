import React, { Component, PropTypes } from 'react'
import { ListItem } from 'material-ui/List';

import AuthorChip from './AuthorChip'

const styles = {
  chip: {
    float: 'right'
  }
}

const AuthoredListItem = (props) => {
  const { text, authorName, authorId, onTouchTap } = props

  const authorIcon = (
    <div style={styles.chip}>
      <AuthorChip
        authorName={authorName}
        authorId={authorId}
      />
    </div>
  )

  return (
    <ListItem
      primaryText={text}
      onTouchTap={e => onTouchTap()}
    >
      {authorIcon}
    </ListItem>
  )
}

export default AuthoredListItem
