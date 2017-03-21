import React, { Component, PropTypes } from 'react'

import Header from '../Header'
import Footer from '../Footer'
import DialogStack from '../dialogs/DialogStack'

const PageHOC = (ChildComponent) => {
  return (props) => {
    const { isAuthenticated } = props

    return (
      <div>
        <Header {...props} />
        <ChildComponent {...props} />
        <Footer {...props} />
        <DialogStack />
      </div>
    )
  }
}

export default PageHOC
