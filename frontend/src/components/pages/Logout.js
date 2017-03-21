import React, { Component, PropTypes } from 'react'

import Dialog from 'material-ui/Dialog'

export default (props) => {
  return (
    <Dialog
      title="You are logged out"
      modal={true}
      open={true}
    />
  )
}
