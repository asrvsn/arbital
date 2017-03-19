import React, { Component, PropTypes } from 'react'

import Chip from 'material-ui/Chip';
import Avatar from 'material-ui/Avatar';

const styles = {
  chip: {
    margin: 4,
  },
}

const goToAuthor = (props, authorId) => {
  props.router.push('/users/' + authorId)
}

const AuthorChip = (props) => {
  const { authorName, authorId } = props

  return (
    <Chip onTouchTap={e =>
            goToAuthor(props, authorId)
          }
          style={styles.chip}>
      <Avatar size={32}>{authorName[0]}</Avatar>
      {authorName}
    </Chip>
  )
}

export default AuthorChip
